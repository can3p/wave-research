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
into port audio. All functions are defined in [wave-research.lisp][4]

~~~lisp
(play-test-audio)
~~~

Hurray! It works. The tricky part was to join cl-wav and cl-portaudio
formats together. cl-portaudio supports only floats, cl-wav can do
both floats and integers, however with integers library produces
the array twice the size. From the source code of cl-wav I understood
that every frame is encoded with two bytes, so when converting to
floats we get one values instead of two. ehm, ok.

Now, since we got a proper sound stream we can try to do some analysis on
top of it.

And now the questions rises - how to analyze this data? I'd really like
to have a look on it at first. After some searching I decided to try my
luck with [eazy-guplot][5] package. Fiddling with images is not cool, so
we'd better get qt terminal:

~~~bash
$ brew install gnuplot --with-qt
~~~

Stackoverflow [provided][6] with a [working snippet][7] for wave visualization.
This snippet expected a huge dat file with data which I don't want to
attach, however it can be easily generated with `sox`:

~~~bash
$ brew install sox
$ sox jumps.wav jumps.dat
$ gnuplot test.plot
~~~

Nice, let's get the same graph from common lisp.

First of all we'll need a time series base on audio data:

~~~lisp
(audio-series (take 10 (read-test-autio-data)))
;; ((0.0 -1.000001) (2.2675736961451248d-5 -1.0000007)
;;  (4.5351473922902495d-5 -1.0000012) (6.802721088435374d-5 -1.0000017)
;;  (9.070294784580499d-5 -1.0) (1.1337868480725624d-4 -0.9999995)
;;  (1.3605442176870748d-4 -1.0000014) (1.5873015873015873d-4 -1.0000021)
;;  (1.8140589569160998d-4 -1.0000007) (2.0408163265306123d-4 -1.0))
~~~

Time step is defined by a sampling rate

Here is a plotting step:

~~~lisp
(plot-audio-data (read-test-audio-data))
~~~

It starts to be somewhat messy there. First of all, common lisp functions are
not polymorphic and whatever works for arrays breaks for lists, so I
had to modify `take-every` function to handle two separate cases. Why was
this function needed in the first place? Because with simple printing
all the values from the stream sbcl chokes and we don't want that.

So, now we see with clarity that beat represents very sharp spikes of
amplitude. My guess is that they should be even sharper if we take a
derivative.

After extensive search I stumbled upon the excellent [talk][8] by Anna Wszeborowska
where she did much more then I needed, and one of the comments had a [link][9]
to the repo. Basically she implemented threshold based detection using FFT
which I remember nothing about except the name. I was worried that the
example is in python and depends on numpy which has no comparable library
in common lisp land, but fortunately not that much was needed - FFT by
itself and hann window function, both were provided by [bordeaux-fft][10] library.

After porting the code I got it working reasonably well, both from audio file and
from microphone. Microphone appeared to be a bit more tricky because I had to
implement an infinite loop to continuously read the data from the mic. Makes a
lot of sense if I think about it now. I had to increase thresholds drastically to
make the analyzer less prone to false positives.

Now, since everything is in place now, we need to get visualization working.
Initially I decided to try out [trivial-gamekit][11] which has a very nice [tutorial][12]

I bet it works awesome for some people, however, sbcl *really* does not want
to play well there without some [tricks][13]. In addition to the most of sdl-based
graphic libraries caused very high cpu usage for me (with notable exception
of trivial-gamekit to be fair), plus using those libraries implied that I'll
do window handling, image loading and lots of other boilerplate on my own
which I did not want to do at all. In the worst case I would have to draw
graphs by myself and the scale of this task started making me really sad.

After a couple of days filled with banging my head against the wall there
I suddenly came to a revelation, that I don't need to go that way at all, since
I already have all-mighty emacs at hand. Quick search proved that my guess was
[right][14] and simple [change][15] to emacs config enabled the desired feature
for me.

Another option and potentially cooler one could be to employ [integration][16] with
junyper notebooks. Well, it would be cooler if we had well-baked ready made
libraries in common lisp world and we don't. Just have a look on all the goodness
that is already [there][17] in python.

By hooking into `print-object` for pathname we can also remove a need in explicit
call of `plot-png` function.

Actually, after reading Allen Downey [book][18] I came to revelation how shockingly
nonconvenient were array operations in lisp by default and I started thinking
if there are any common lisp libraries that ease the pain.

[1]: https://github.com/can3p/wave-research/blob/master/jumps.wav
[2]: https://github.com/filonenko-mikhail/cl-portaudio
[3]: https://github.com/RobBlackwell/cl-wav
[4]: https://github.com/can3p/wave-research/blob/master/src/wave-research.lisp
[5]: https://github.com/guicho271828/eazy-gnuplot
[6]: https://stackoverflow.com/questions/5826701/plot-audio-data-in-gnuplot
[7]: https://github.com/can3p/wave-research/blob/master/test.plot
[8]: https://www.youtube.com/watch?v=at2NppqIZok
[9]: https://github.com/aniawsz/rtmonoaudio2midi
[10]: http://vintage-digital.com/hefner/software/bordeaux-fft/manual.html
[11]: https://borodust.org/projects/trivial-gamekit
[12]: https://borodust.org/projects/trivial-gamekit/getting-started/
[13]: https://github.com/lispgames/cl-sdl2/issues/89
[14]: https://righele.it/2012/02/22/display-images-in-slime-from-common-lisp/
[15]: https://github.com/can3p/emacs.d/commit/a49fbd52ef5f1e59ada016efc9c7e4b15178d468
[16]: https://github.com/fredokun/cl-jupyter/
[17]: http://nbviewer.jupyter.org/github/AllenDowney/ThinkDSP/blob/master/code/chap11.ipynb
