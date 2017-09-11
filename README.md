# What is it like to work with sound in Common Lisp?

The purpose of this exercise is to learn more about ways to do
audio processing in common lisp. Please take into account that
I have absolutely no idea about what I'm doing, so this document
basically documents my findings.

First of all, the task. My aim is to be able to process the sound
from microphone to recognize some pattern there. I've attached
a [file][1] as an example.

Before jumping in more technical stuff I decided to figure out
what actually reading and playing the sound looks like. From
what I understand the sound is basically a wave that is split
to a particular number of samples (according to sampling rate).
So the result of the read is a long array of numbers (can be
different ones - ints, floats from -1.0 to 1.0 etc). Multiple
channels just mean multiple arrays and that's it.

For reading/writing sound from the devices [cl-portaudio][2] looks
like the best choice. It's already set as a dependency to this
system, so we can just load it and switch to the `wave-research`
package.

~~~lisp
(ql:quickload :wave-research)
(in-package wave-research)
~~~

Now we should be able to run a test example from cl-portaudio:

~~~lisp
(portaudio-tests:test-read-write-converted-echo)
~~~

At this point it's already clear that portaudio works and we
can move on. Next step is to read the audio file and to output
it. That won't be necessary for the analysis, but at least we'll
know that we can do that, right? For that I've found nothing
except [cl-wav][3] library.

I defined a function `read-test-files` that will read the contents
of test file! Resulting structure consists of three elements. First
two are metadata, and the last one is an actual enourmous array
of bytes.

~~~lisp
(getf (caddr (read-test-file)) :chunk-data-size)
;; 3956072

(take 20 (getf (caddr (read-test-file)) :chunk-data))
;; #(252 255 253 255 251 255 249 255 0 0 2 0 250 255 247 255 253 255 0 0)
~~~

Now that we now what kind of data we get we can try to feed it
into port audio.

Here is an initial try. Sound is horrible, which means I'm sending
data wrong.

[1]: https://github.com/can3p/wave-research/blob/master/jumps.wav
[2]: https://github.com/filonenko-mikhail/cl-portaudio
[3]: https://github.com/RobBlackwell/cl-wav
