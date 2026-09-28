# ================================================================
# LIMPIEZA PROFUNDA DE VOZ
#
# Entrada:
#     MP3 / WAV / M4A / FLAC
#
# Salidas:
#     voz_limpia.wav
#     voz_limpia.mp3
#
# Procesamiento:
#     1. Conversión a WAV
#     2. DeepFilterNet
#     3. Filtro pasa-altos
#     4. Control espectral
#     5. Voice gate
#     6. Compresión suave
#     7. Normalización
#
# ================================================================

from pathlib import Path
import subprocess
import shutil
import sys

import numpy as np
import soundfile as sf
from scipy import signal


# ================================================================
# CONFIGURACIÓN
# ================================================================

INPUT_FILE = "AUDIO PROPIO.mp3"

OUTPUT_DIR = Path("resultado")

# Frecuencia de trabajo
TARGET_SR = 48000

# ------------------------------------------------
# FILTRO
# ------------------------------------------------

# Elimina ruidos muy graves:
# vibraciones, golpes de mesa, tráfico grave, etc.
HP_CUTOFF = 70

# Filtro pasa bajos.
# Evita exagerar hiss y ruido de alta frecuencia.
LP_CUTOFF = 14000


# ------------------------------------------------
# GATE
# ------------------------------------------------

# Umbral relativo para considerar que hay voz.
#
# IMPORTANTE:
# Si se corta parte de la voz, bajar este valor.
#
# Si queda demasiado ruido durante silencios,
# subirlo.
#
GATE_THRESHOLD_DB = -42

# Tiempo mínimo que debe durar un segmento
# para considerarlo voz.
MIN_SPEECH_MS = 80

# Suavizado de entrada/salida del gate
ATTACK_MS = 8
RELEASE_MS = 120


# ------------------------------------------------
# COMPRESIÓN
# ------------------------------------------------

COMPRESSOR_THRESHOLD_DB = -20
COMPRESSOR_RATIO = 2.0


# ------------------------------------------------
# NORMALIZACIÓN
# ------------------------------------------------

TARGET_PEAK_DB = -1.0


# ================================================================
# UTILIDADES
# ================================================================

def run_command(command):

    print("\nEjecutando:")
    print(" ".join(map(str, command)))

    result = subprocess.run(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )

    print(result.stdout)

    if result.returncode != 0:
        raise RuntimeError(
            f"El comando falló con código {result.returncode}"
        )


# ================================================================
# COMPROBAR FFmpeg
# ================================================================

def check_ffmpeg():

    if shutil.which("ffmpeg") is None:

        raise RuntimeError(
            "\nFFmpeg no está instalado o no está en PATH.\n"
            "Probá ejecutar:\n\n"
            "    ffmpeg -version\n"
        )


# ================================================================
# CONVERTIR AUDIO A WAV
# ================================================================

def convert_to_wav(input_file, output_file):

    run_command([
        "ffmpeg",
        "-y",
        "-i",
        str(input_file),

        # mono
        "-ac",
        "1",

        # frecuencia
        "-ar",
        str(TARGET_SR),

        # WAV PCM 32 bit float
        "-c:a",
        "pcm_f32le",

        str(output_file)
    ])


# ================================================================
# CARGAR AUDIO
# ================================================================

def load_audio(path):

    audio, sr = sf.read(
        path,
        dtype="float32"
    )

    if audio.ndim > 1:
        audio = np.mean(audio, axis=1)

    return audio, sr


# ================================================================
# FILTRO BUTTERWORTH
# ================================================================

def butter_filter(audio, sr, cutoff, btype):

    nyquist = sr / 2

    normalized = cutoff / nyquist

    sos = signal.butter(
        4,
        normalized,
        btype=btype,
        output="sos"
    )

    return signal.sosfiltfilt(
        sos,
        audio
    )


# ================================================================
# FILTRO DE VOZ
# ================================================================

def voice_band_filter(audio, sr):

    print("\n[3] Filtrando frecuencias innecesarias...")

    # Elimina rumble
    audio = butter_filter(
        audio,
        sr,
        HP_CUTOFF,
        "highpass"
    )

    # Limita ruido ultrasónico/agudos extremos
    audio = butter_filter(
        audio,
        sr,
        LP_CUTOFF,
        "lowpass"
    )

    return audio


# ================================================================
# RMS
# ================================================================

def frame_rms(audio, frame_length):

    n_frames = len(audio) // frame_length

    rms = np.zeros(n_frames)

    for i in range(n_frames):

        frame = audio[
            i * frame_length:
            (i + 1) * frame_length
        ]

        rms[i] = np.sqrt(
            np.mean(frame ** 2) + 1e-12
        )

    return rms


# ================================================================
# dB
# ================================================================

def amplitude_to_db(x):

    return 20 * np.log10(
        np.maximum(x, 1e-10)
    )


# ================================================================
# GATE INTELIGENTE
# ================================================================

def noise_gate(audio, sr):

    print("\n[4] Aplicando voice gate...")

    # Ventana de análisis
    frame_ms = 20

    frame_length = int(
        sr * frame_ms / 1000
    )

    if frame_length < 1:
        return audio

    rms = frame_rms(
        audio,
        frame_length
    )

    db = amplitude_to_db(rms)

    # Umbral
    threshold = GATE_THRESHOLD_DB

    speech = db > threshold

    # ------------------------------------------------
    # Eliminar segmentos extremadamente cortos
    # ------------------------------------------------

    min_frames = max(
        1,
        int(
            MIN_SPEECH_MS /
            frame_ms
        )
    )

    speech_clean = speech.copy()

    i = 0

    while i < len(speech):

        if speech[i]:

            start = i

            while (
                i < len(speech)
                and speech[i]
            ):
                i += 1

            end = i

            if end - start < min_frames:

                speech_clean[
                    start:end
                ] = False

        else:

            i += 1

    # ------------------------------------------------
    # Convertir máscara de frames a samples
    # ------------------------------------------------

    mask = np.repeat(
        speech_clean,
        frame_length
    )

    mask = mask[:len(audio)]

    # Si faltan muestras
    if len(mask) < len(audio):

        mask = np.pad(
            mask,
            (
                0,
                len(audio) - len(mask)
            ),
            constant_values=False
        )

    mask = mask.astype(np.float32)

    # ------------------------------------------------
    # Suavizado de ataque
    # ------------------------------------------------

    attack_samples = int(
        sr * ATTACK_MS / 1000
    )

    release_samples = int(
        sr * RELEASE_MS / 1000
    )

    # Envelope
    envelope = np.zeros_like(mask)

    current = 0.0

    for i in range(len(mask)):

        target = mask[i]

        if target > current:

            step = 1.0 / max(
                1,
                attack_samples
            )

            current = min(
                1.0,
                current + step
            )

        else:

            step = 1.0 / max(
                1,
                release_samples
            )

            current = max(
                0.0,
                current - step
            )

        envelope[i] = current

    return audio * envelope


# ================================================================
# COMPRESOR SUAVE
# ================================================================

def soft_compressor(
    audio,
    threshold_db=-20,
    ratio=2.0
):

    print("\n[5] Aplicando compresión suave...")

    sign = np.sign(audio)

    magnitude = np.abs(audio)

    threshold = 10 ** (
        threshold_db / 20
    )

    above = magnitude > threshold

    compressed = magnitude.copy()

    compressed[above] = (
        threshold
        +
        (
            magnitude[above] - threshold
        ) / ratio
    )

    return compressed * sign


# ================================================================
# NORMALIZACIÓN
# ================================================================

def normalize_peak(
    audio,
    target_db=-1
):

    peak = np.max(
        np.abs(audio)
    )

    if peak <= 0:
        return audio

    target = 10 ** (
        target_db / 20
    )

    gain = target / peak

    print(
        f"\n[6] Ganancia de normalización: "
        f"{20*np.log10(gain):.2f} dB"
    )

    return audio * gain


# ================================================================
# LIMITADOR DE SEGURIDAD
# ================================================================

def limiter(audio):

    return np.clip(
        audio,
        -1.0,
        1.0
    )


# ================================================================
# GUARDAR WAV
# ================================================================

def save_wav(
    path,
    audio,
    sr
):

    sf.write(
        path,
        audio.astype(np.float32),
        sr,
        subtype="PCM_24"
    )


# ================================================================
# WAV → MP3
# ================================================================

def wav_to_mp3(
    wav_file,
    mp3_file
):

    run_command([
        "ffmpeg",
        "-y",
        "-i",
        str(wav_file),

        "-codec:a",
        "libmp3lame",

        "-b:a",
        "320k",

        str(mp3_file)
    ])


# ================================================================
# DEEPFILTERNET
# ================================================================

def deepfilternet_enhance(
    input_wav,
    output_wav
):

    print("\n")
    print("=" * 60)
    print("DEEPFILTERNET")
    print("=" * 60)

    try:

        from df.enhance import (
            enhance,
            init_df,
            load_audio,
            save_audio
        )

    except ImportError:

        raise RuntimeError(
            "\nNo se pudo importar DeepFilterNet.\n\n"
            "Instalalo con:\n\n"
            "    pip install deepfilternet\n"
        )

    print(
        "\nInicializando modelo..."
    )

    model, df_state, _ = init_df()

    print(
        "Modelo cargado."
    )

    audio, _ = load_audio(
        str(input_wav),
        sr=df_state.sr()
    )

    print(
        "Procesando voz..."
    )

    enhanced = enhance(
        model,
        df_state,
        audio
    )

    save_audio(
        str(output_wav),
        enhanced,
        df_state.sr()
    )

    print(
        f"DeepFilterNet terminado:\n"
        f"{output_wav}"
    )


# ================================================================
# PROGRAMA PRINCIPAL
# ================================================================

def main():

    print("\n")
    print("=" * 70)
    print("        LIMPIEZA PROFUNDA DE VOZ")
    print("=" * 70)

    # ------------------------------------------------
    # Comprobaciones
    # ------------------------------------------------

    check_ffmpeg()

    input_file = Path(INPUT_FILE)

    if not input_file.exists():

        raise FileNotFoundError(
            f"\nNo existe:\n{input_file.resolve()}"
        )

    OUTPUT_DIR.mkdir(
        exist_ok=True
    )

    # ------------------------------------------------
    # Archivos temporales
    # ------------------------------------------------

    original_wav = (
        OUTPUT_DIR /
        "01_original.wav"
    )

    enhanced_wav = (
        OUTPUT_DIR /
        "02_deepfilter.wav"
    )

    processed_wav = (
        OUTPUT_DIR /
        "03_procesado.wav"
    )

    final_wav = (
        OUTPUT_DIR /
        "voz_limpia.wav"
    )

    final_mp3 = (
        OUTPUT_DIR /
        "voz_limpia.mp3"
    )

    # ------------------------------------------------
    # 1. Convertir
    # ------------------------------------------------

    print("\n[1] Convirtiendo audio...")

    convert_to_wav(
        input_file,
        original_wav
    )

    # ------------------------------------------------
    # 2. DeepFilterNet
    # ------------------------------------------------

    print(
        "\n[2] Eliminando ruido mediante "
        "DeepFilterNet..."
    )

    deepfilternet_enhance(
        original_wav,
        enhanced_wav
    )

    # ------------------------------------------------
    # 3. Cargar resultado
    # ------------------------------------------------

    audio, sr = load_audio(
        enhanced_wav
    )

    print(
        f"\nFrecuencia de muestreo: "
        f"{sr} Hz"
    )

    print(
        f"Duración: "
        f"{len(audio)/sr:.2f} segundos"
    )

    # ------------------------------------------------
    # 4. Filtrado
    # ------------------------------------------------

    audio = voice_band_filter(
        audio,
        sr
    )

    # ------------------------------------------------
    # 5. Voice gate
    # ------------------------------------------------

    audio = noise_gate(
        audio,
        sr
    )

    # ------------------------------------------------
    # 6. Compresión
    # ------------------------------------------------

    audio = soft_compressor(
        audio,
        threshold_db=COMPRESSOR_THRESHOLD_DB,
        ratio=COMPRESSOR_RATIO
    )

    # ------------------------------------------------
    # 7. Normalización
    # ------------------------------------------------

    audio = normalize_peak(
        audio,
        TARGET_PEAK_DB
    )

    # ------------------------------------------------
    # 8. Limiter
    # ------------------------------------------------

    audio = limiter(
        audio
    )

    # ------------------------------------------------
    # Guardar intermedio
    # ------------------------------------------------

    save_wav(
        processed_wav,
        audio,
        sr
    )

    # ------------------------------------------------
    # WAV FINAL
    # ------------------------------------------------

    shutil.copyfile(
        processed_wav,
        final_wav
    )

    # ------------------------------------------------
    # MP3 FINAL
    # ------------------------------------------------

    print(
        "\n[7] Generando MP3..."
    )

    wav_to_mp3(
        final_wav,
        final_mp3
    )

    # ------------------------------------------------
    # Comprobación duración
    # ------------------------------------------------

    final_audio, final_sr = load_audio(
        final_wav
    )

    duration_original = (
        len(audio) / final_sr
    )

    print("\n")
    print("=" * 70)
    print("PROCESAMIENTO TERMINADO")
    print("=" * 70)

    print(
        f"\nWAV:\n{final_wav.resolve()}"
    )

    print(
        f"\nMP3:\n{final_mp3.resolve()}"
    )

    print(
        f"\nDuración final: "
        f"{duration_original:.2f} s"
    )

    print("\n")


# ================================================================
# EJECUTAR
# ================================================================

if __name__ == "__main__":

    try:

        main()

    except Exception as e:

        print("\n")
        print("=" * 70)
        print("ERROR")
        print("=" * 70)

        print(
            f"\n{e}"
        )

        sys.exit(1)
