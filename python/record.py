import pyaudio
import librosa
import numpy as np
import requests
import time;

class RingBuffer():
    "A 1D ring buffer using numpy arrays"
    def __init__(self, length):
        self.data = np.zeros(length, dtype='f')
        self.index = 0

    def extend(self, x):
        "adds array x to ring buffer"
        x_index = (self.index + np.arange(x.size)) % self.data.size
        self.data[x_index] = x
        self.index = x_index[-1] + 1

    def get(self):
        "Returns the first-in-first-out data in the ring buffer"
        idx = (self.index + np.arange(self.data.size)) %self.data.size
        return self.data[idx]

# ring buffer will keep the last 2 seconds worth of audio
ringBuffer = RingBuffer(30 * 22050)
sr = 22050

def callback(in_data, frame_count, time_info, flag):
    audio_data = np.fromstring(in_data, dtype=np.float32)

    # we trained on audio with a sample rate of 22050 so we need to convert it
    audio_data = librosa.resample(audio_data, 44100, 22050)
    ringBuffer.extend(audio_data)

    # 3. Run the default beat tracker
    tempo, beat_frames = librosa.beat.beat_track(y=ringBuffer.get(), sr=sr)
    print("Beat frames: ", beat_frames)

    if len(beat_frames) > 0:
        print('Estimated tempo: {:.2f} beats per minute'.format(tempo))

        # 4. Convert the frame indices of beat events into timestamps
        beat_times = librosa.frames_to_time(beat_frames, sr=sr)
        beat_samples = librosa.frames_to_samples(beat_frames)

        print('detected beat samples', beat_samples)
        print('detected beat frames', beat_frames)
        print('detected beat times', beat_times)


    return (in_data, pyaudio.paContinue)

pa = pyaudio.PyAudio()


stream = pa.open(format = pyaudio.paFloat32,
                 channels = 1,
                 rate = 44100,
                 output = True,
                 input = True,
                 stream_callback = callback)

# start the stream
stream.start_stream()

print "Recording started"

while stream.is_active():
    time.sleep(0.25)

stream.close()
pa.terminate()
