<div id="wrap">
<div id="navigation">CSOUND JOURNAL<a href="index.html"></a>
</div>

<div id="header">
## Speed up your Csound work-flow with Haskell

### Introduction to Csound-expression
</div>


<div id="content">
    
## Introduction

The Csound-expression is the Haskell framework for computer music production.
It creates the Csound programs out of Haskell programs. It can greatly speed up 
the text-based development of computer music and synthesizers.

The Haskell is a purely functional programming language. It means that a program
is made out of functions and compositions of functions. It's a modern language that
features many cutting edge concepts of Computer science.

Why should Csounders bother on a new language like Haskell? The price of learning new language is pretty high,
but it can give you a expressive power that's hard to imagine within the Csound syntax. 

Imagine that you can 

* write a synthesizer in a single line of code.

* develop synthesizers right in the REPL. You can type in a line of
    code press enter and you get the sound out of your speakers as a feedback. Then you can
    fix some values type a line or to again and then you can hear the sound. 

* pass a filter function as a parameter or create a list
    of functions and pass them around as values. 

* store a score section as a value and then you can append
    more notes to it inside another function. 

* create compound data structures and you can easily redefine opcodes
    to take default values. We can hide away all those parameters that we set up
     not so frequently. 

* use beautiful predefined instruments. There is a collection of patches ready to be used.

* create reusable libraries of synthesizers.

* imagine that many things are derived from the context or set up to sensible defaults. 


The library csound-expression (CE for short) is based on several main principles:

* everything is an expression. We can create all parts from more simple and primitive expressions
    and we can pass compound and primitive values as values. We can even pass around UI-widgets 
    as values!

* prefer convention over configuration, using context as much as possible to derive the useful behavior.     

But let the code speak for itself. Here is the Hello World program:

~~~haskell
> dac (osc 440)
~~~

That's all we need to write to get the audio going! The function `dac` sends the signal to 
speakers and the `osc` creates a pure sine wave.

In the haskell we apply function to arguments and use spaces as delimiters:

~~~haskell
g (f a1 a2 a3) b2 
~~~

We can use parenthesis to group the values. The `(f a1 a2 a3)` is the same as we write it in the Csound but
without commas. So in the previous example the function `g` is applied to two arguments. 
The first one is `(f a1 a2 a3)` and the second one is `b2`. So recalling our first example we 
apply the function `osc` to the frequency `440` an we pass the result to the function `dac` (short for 
digital to analog converter). The naming here is borrowed from Pure Data. 

That's it! It's a complete program! We have the default settings for rates and the number of output 
channels is derived from the input of the `dac` function. For example we can make it a stereo by passing 
a pair:

~~~haskell
> dac (osc 440, saw 220)
~~~

It's better to hear the output and not just look at the code. So let's setup everything we need.
To change the defaults we can use the function `dacBy`:

~~~haskell
> let run x = dacBy (setRates 48000 128) x

> run (osc 440, saw 220)
~~~

When the line is run the function `dac` creates a file `tmp.csd` in the current directory
with Csound code and invokes Csound on it.

### Installation guide

The library Csound-expression is distributed with `Cabal`. It's a standard way 
to share the libraries and applications in the Haskell community. The `cabal`
is like `pip` for Python or `npm` for `Node` `js`. The library is hosted on Hackage. Its the main repository
of Haskell open source software. They are called packages. The cabal is going to check the Hackage
for libraries and install them on demand (resolving dependencies, creating docs etc). 

So we need the GHC (the Haskell compiler) and the Cabal (haskell package distribution system). 
And for sure we need the Csound. The recommended version is 6.05 or higher. But it also can run on previous
versions too. The 5.17 is the desired minimum. But the more modern Csound you use the more features is available to you. 

I guess that the Csound is alredy installed on your system. The easiest way to get the Haskell components
is to install the [Haskell Platform](https://www.haskell.org/platform/).
When it's installed we can install the library.

Execute in the command line:

~~~
> cabal update
~~~

To fetch the updates. Type to install the bare essentials:

~~~
> cabal install csound-expression
~~~

Type to install the batteries:

~~~
> cabal install csound-catalog
~~~

It includes ready to use synthesizers and functions to compose the music with clips
aligned with BPM. 

## I. Fist steps with the library

In this section we are going to study the most interesting features of the library.
Features are introduced with examples. The library is rather big so my point in this section
is not to give the complete description of it but to show the most useful tools for 
a performing musician and composer. We are going to learn how to create simple drones, 
how to practice with metronome and create complex beats in few lines of code. 
How to record performance and incorporate the recorded audio in the live gig.
We are going to play some beautiful patches with midi devices and encounter 
unusual ancient tunings.

### Hello World!

Now we can open the Haskell REPL called ghci (GHC interpreter for short) import the library 
and type the Hello world program:

~~~haskell
ghci
> import Csound.Base
> dac (osc 440)
~~~

Press `Ctrl+C` to stop the playback.

We can get the fancier sound with functions `testDrone`, `testDrone2`, `testDrone3`, `testDrone4`:

~~~haskell
> dac (testDrone 220)
~~~

Also we can use Csound pitch class to specify the frequency:

~~~haskell
> dac (testDrone (cpspch 7.00))
~~~

We can add several signals to create a chord:

~~~haskell
> dac (testDrone (cpspch 7.00) + testDrone (cpspch 7.07))
~~~

The output is too loud we can make it quiter by scaling the amplitude of the signal with
function `mul`:

~~~haskell
> dac (mul 0.3 (testDrone (cpspch 7.00) + testDrone (cpspch 7.07)))
~~~

We can add signals with the function `sum`. It takes in a list of values and sums them up:

~~~haskell
> dac (mul 0.3 (sum [testDrone (cpspch 7.00), testDrone (cpspch 7.07), testDrone (cpspch 8.04)]))
~~~

The Haskell lists are enclosed into square brackets: `[1, 2, 3]`. Tuples are enclosed in parenthesis: `(a, b)`.

We can see some duplication is going on. We apply the same combo of functions to all components
in the list. We apply the composition of functions `testDrone` and `cpspch`. In Haskell
we can compose the functions on the fly with the operator `dot`:

~~~haskell
f x = testDrone (cpspch x)   ===   f = testDrone . cpspch
~~~

To apply the same functions to all elements in the list we can use the function `fmap`:

~~~haskell
[f x, f y, f z]    ===  fmap f [x, y, z]
~~~

Keeping that in mind we can rewrite our chord like this:

~~~haskell
> dac (mul 0.3 (sum (fmap (testDrone . cpspch) [7.00, 7.07, 8.04])))
~~~

Here we can see a glimpse of functional programming in action. With simple operator
we have combined two functions and applied it to a list of values. 
We can make the expression more readable if we introduce local values:

~~~haskell
> let signals = fmap (testDrone . cpspch) [7.00, 7.07, 8.04]
> dac (mul 0.3 (sum signals))
~~~ 

We introduce a variable with syntax:

~~~haskell
let value = expression
~~~

Notice that this syntax works only in the interpreter. In the compiled files
we can just write:

~~~haskell
value = expression
~~~

### Adjusting the volume

We can adjust the volume with function `mul`. It takes a signal as the first argument
and anything that can be scaled with signals as a volume. It can be a simple signal
or a tuple of signals or it can be a UI-widget that produces the signals.

Let's adjust a volume for out chord:

~~~haskell
> dac (mul 0.36 (sum signals))
~~~

The volume value is the signal itself. We can control it with LFO:

~~~haskell
> dac (mul (0.3 * uosc 1) (sum signals))
~~~

The function `uosc` produces unipolar pure sine signal (ranges from 0 to 1).

### Metronome click

We know how to create chords. Can we augment the harmony with the rhythm?
We can create a simple metronome click with the function `ticks`:

~~~haskell
> dac (ticks 4 120)
~~~

We can change the timbre with functions `ticks2`, `ticks3`, `ticks4`.
Also we can create more complicated rhythms with the function `nticks`.
It takes a list of beat measures instead of single measure. We can create a 7/8 beat
like this:

~~~haskell
> dac (nticks [2, 2, 3] 160)
~~~

Let's combine the metronome with rhythm:

~~~haskell
> let drone = mul 0.3 (sum (fmap (testDrone . cpspch) [7.00, 7.07, 8.04]))

> let rhythm = nticks [2, 2, 3] 160

> dac (sum [drone, rhythm])

<interactive>:12:18:
    Couldn't match expected type ‘SE Sig2’ with actual type ‘Sig’
    In the expression: rhythm
    In the first argument of ‘sum’, namely ‘[drone, rhythm]’
~~~

We get an error. Why does it happen? We can sum only values of the same type.
But our values `drone` and `rhythm` have different types. We can check the type of
any value in the interpreter with command `:t value`

~~~haskell
> :t drone
drone :: SE Sig2

> :t rhythm
rhythm :: Sig
~~~

We can see that the `rhythm` has type of `Sig`. It's a plain signal or a stream of floats.
It can be audio or control rate the actual Csound type is derived from the context. 
In the case of `rhythm` it is an audio signal. The type of `drone` is more interesting. 
It's a pair of signals that is wrapped in the special type `SE`. 

So we need to convert the simpler type of `Sig` to `SE Sig2`. 
We can convert mono audio to stereo with function

~~~haskell
> :t fromMono
fromMono :: Sig -> (Sig, Sig)
~~~

### Introduction to side effects

But we also need to wrap the value to `SE`. The `SE` is short for side-effects.
The expression `SE a` means that the type `SE` is parametrized with some type of `a`.
Like lists or arrays have certain structure but the type of elements can be anything
as long as they are organized in a certain way.
Now we are landing at the zone that is unique to Haskell. The Haskell is a pure language.
It's pure in mathematical sense. The pureness means that if we assign the expression to the value
we can safely substitute the value with assigned expression anywhere in the code. 
This seems to be obvious feature to have but not in the programming world. Almost all
languages break this assumption. Consider the code:

~~~haskell
a = getRandomInt 
b = a + a
~~~

With the notion of pureness we can safely substitute the value with the definition

~~~haskell
b = getRandomInt + getRandomInt
~~~

But it's quite different program. Most languages break the rule of pureness.
They force the execution from top to bottom line by line. But in Haskell the order of execution
is different. The expressions are executed by functional dependencies. The compiler executes the 
top most expression it looks at the definition and substitutes all values with it's definitions then
it founds other compound values and substitutes them with definitions and so on when there are only primitive 
values left.It's a simplified model of execution. The real model is a bit more complicated. 
It executes subexpressions lazily. It means that it caches the values so that we don't need to compute them twice.

But how do we use random values in Haskell. The randomness breaks the purity. In Haskell there is a special
type called with kind of scary name `Monad`. There are many monad tutorials perhaps too many of them.
You can read on this topic [here](https://github.com/anton-k/monads-for-drummers) or [there](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html).

Right now it's good to know that there is a special syntax in Haskell to handle the impure code.
It's called a `do`-notation:

~~~haskell
once = do
    a <- getRandomInt
    return (a + a)

twice = do
    a1 <- getRandomInt
    a2 <- getRandomInt
    return (a1 + a2)
~~~

With it we can distinguish those two cases. In the do-notation the lines are executed from top to bottom
one by one just like in most programming languages. 

The type of impure value is marked with a wrapper. This type wrapper is a Monad if it supports certain operations.
There are two of them:

~~~haskell
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
~~~

The return wraps pure value `a` to the monadic one `m a`. The operator bind `>>=`
applies a monadic value `m a` to a function that returns a monadic value `m b`.

In the CE library all impure values are wrapped in the type `SE`. The type `SE Sig2` for drone means
that we use randomness somewhere inside our synthesizer. So turning back to our task
to unify `Sig` with `SE Sig2`. We use the function `fromMono` to convert mono signal to stereo
and we use `return` to wrap the value and then we can sum them up:

~~~haskell
> dac (sum [drone, return (fromMono rhythm)])
~~~

We can adjust the volumes with function `mul`:

~~~haskell
> dac (sum [drone, mul 1.3 (return (fromMono rhythm))])
~~~

### The dollar operator

As our expressions become more involved it's good to introduce a useful operator
that can save us some typing. It's a dollar operator `$`. It's an application of function
to value just like the space. It has the lowest order of precedence and space has the highest one.

The dollar sign lets us skip tons of parenthesis in expressions like:

~~~haskell
> dac (mul 0.5 (osc (440 * uosc 0.1)))
~~~

With the help of dollars we can rewrite it like this:

~~~haskell
> dac $ mul 0.5 $ osc $ 440 * uosc 0.1
~~~

So the essence of the dollar can be expressed in the equation:

~~~haskell
f (g a)   ===  f $ g a
~~~

### Let's add some cool synthesizers

Many beautiful instruments are ready to use (package `csound-catalog`):

~~~haskell
> import Csound.Patch

> dac $ atMidi toneWheelOrgan

> dac $ mul 0.45 $ atMidi dreamPad

> dac $ mul 0.45 $ atMidi $ vibhu 65  -- needs Csound 6.05 or higher
~~~

The function `atMidi` takes in a `Patch` and applies the patch to the stream of midi messages.

~~~haskell
atMidi :: Patch Sig2 -> SE Sig2
~~~

You can see the `SE` wrapper in the output. It's used because we read the values from the user input.
So the value is not fixed or pure and depends on the creativity of the user.

With `dac` we listen for messages from the real MIDI-device. If you don't have the MIDI-keyboard try out `vdac`. 
It creates a virtual keyboard to test the synthesizer.

The function `vdac` creates virtual MIDI-keyboard:

~~~haskell
> vdac $ mul 0.3 $ atMidi dreamPad
~~~

#### Non-equal temperaments

The interesting feature of the patches is that they are defined on frequencies not on midi pitches.
We can specify our own conversion from midi-pitches to frequencies. The default behavior is to use the
equal temperament. But with the function `atMidiTemp` we can supply our own temperaments.
There are some predefined ones to use: meantone, werckmeister, pythagor, young1, young2.

We can listen to the music as the Bach have listened to it:

~~~haskell
> vdac $ atMidiTemp werckmeister harpsichord
~~~

#### There are many more synthesizers

You can try some patches from the list:

~~~
cathedralOrgan      dreamPad          noiz               whaleSongPad
vibraphone2         xylophone         simpleMarimba      bassClarinet
razorLead           fmDroneMedium     hammondOrgan       overtonePad
choirA              scrapeDahina      pwEnsemble         hulusi
epiano1             chalandiPlates    banyan             nightPad
~~~

We can find out the whole list of patches  in the module `Csound.Patch` of the
package csound-catalog. See [Csound.Patch](https://hackage.haskell.org/package/csound-catalog-0.5.0/docs/Csound-Patch.html).

### Beat making

Let's substitute the metronome with drums! We have a collection of predefined drums. 
Right now we can find three collections in the package csound-catalog. Also we can use audio
files as drum samples.

~~~haskell
> import Csound.Catalog.Drum.Tr808
~~~

Let's start with three sounds:

~~~haskell
bd    - base drum           sn - snare drum         chh - closed high hat
~~~

Let's listen to them:

~~~haskell
> dac bd

> dac sn

> dac chh
~~~


#### Creating patterns

We can use the module `Csound.Sam` to arrange the music from clips that are
aligned with bpm:

~~~haskell
> import Csound.Sam
~~~

##### Euclidean beats

There is a very simple way to create quite complicated beats. 
We can create so called Euclidean beats with function `pat` (short for pattern). 

~~~haskell
> dac $ pat [3, 3, 2] bd

> dac $ pat [2, 1, 1] chh
~~~

##### Delaying the clips

We can delay the sample by the number of beats with the function `del` (short for delay):

~~~haskell
> dac $ sum [         pat [3, 3, 2] bd
            , del 2 $ pat [4]       sn ]
~~~

For readability I write it on several lines but you should type it in the single line like this:

~~~haskell
> dac $ sum [ pat [3, 3, 2] bd, del 2 $ pat [4] sn ]
~~~

##### Changing the speed 

We can change the speed of playback with the function `str` (short for stretch).

~~~haskell
> dac $ str 0.5 $ sum [ pat [3, 3, 2] bd, del 2 $ pat [4] sn ]
~~~

##### Introduce the accents

When all samples are played with the same volume it quickly becomes too boring to listen.
We can specify the accents with the function `pat'`. For example let's add a hi-hats:

~~~haskell
> dac $ str 0.5 $ pat' [1, 0.5, 0.2, 0.1] [1] chh
~~~

Notice that the first list is the list of volumes and the second is the list of beats.

Let's play them together:

~~~haskell
> dac $ str 0.5 $ 
        sum [         pat [3, 3, 2] bd
            , del 2 $ pat [4]       sn
            ,         pat' [1, 0.5, 0.2, 0.1] [1] chh ]
~~~

Let's add some toms that happen at the odd places:

~~~haskell
> let drums = str 0.5 $ 
              sum   [          pat [3, 3, 2] bd
                    , del 2  $ pat [4]       sn
                    ,          pat' [1, 0.5, 0.2, 0.1] [1] chh 
                    , del 3  $ pat [5, 11, 7, 4] mtom
                    ,          pat [4, 7, 1, 9]  htom
                    , del 7  $ pat [3, 7, 6] ltom
                    , del 16 $ pat [15, 2, 3] rim
                    ]
> dac drums
~~~

##### Adjusting the volume of the samples

We can also adjust the volumes of samples with the function `mul` just like we did it with signals
or tuples of signals:

~~~haskell
> let drums = str 0.5 $ 
               sum  [          pat [3, 3, 2] bd
                    , del 2  $ pat [4]       sn
                    ,          pat' [1, 0.5, 0.2, 0.1] [1] chh 
                    , mul 0.25 $ sum [
                        del 3  $ pat [5, 11, 7, 4] mtom
                      ,          pat [4, 7, 1, 9]  htom
                      , del 7  $ pat [3, 7, 6] ltom]
                    , del 16 $ pat [15, 2, 3] rim
                    ]
> dac drums
~~~

One-liner for copy and paste:

~~~haskell
> let drums = str 0.5 $ sum  [ pat [3, 3, 2] bd, del 2  $ pat [4] sn, pat' [1, 0.5, 0.2, 0.1] [1] chh, mul 0.25 $ sum [ del 3  $ pat [5, 11, 7, 4] mtom, pat [4, 7, 1, 9]  htom, del 7  $ pat [3, 7, 6] ltom], del 16 $ pat [15, 2, 3] rim]
~~~

##### Other samples

You can try to create your own beats with other drum samples. 
Here is the list of the samples available in the Tr808 module:

~~~haskell
bd, bd2 -  base drums                 htom, mtom, ltom   - high middle low toms
sn      -  snare                      cl                 - claves 
chh     -  closed high-hat            rim                - rim-shot
ohh     -  open high-hat              mar                - maracas
cym     - cymbal                      hcon, mcon, lcon   - high, middle, low conga
~~~

We can also try out other drum collections defined in the modules `Csound.Catalog.Drum.Hm` 
and `Csound.Catalog.Drum.MiniPops` (see the docs at the hackage page for the package csound-catalog).

##### Limit the duration of the sample

So far all our samples were infinite. But what if we want to alternate
the hi-hats with the moments of silence? we can limit the duration of the sample
with the function `lim`:

~~~haskell
lim :: D -> Sam -> Sam
~~~

The first argument `D` is the constant number of beats to cut the sample (can be floating number).
The `Sam` is the type for samples.

Let's play the hi-hats only for 8 beats:

~~~haskell
> dac $ lim 8 $ pat' [1, 0.5, 0.2, 0.1] [1] chh 
~~~

#### Play one pattern after another

We can stack patterns in line with the function `mel` (sort for melody):

~~~haskell
mel :: [Sam] -> Sam
~~~

It takes a list of samples and plays them one after another. Let's play three toms and snare
one after another:

~~~haskell
> dac $ mel [htom, mtom, ltom, sn]
~~~

##### Playing loops

What if we want to repeat the sequence of four kicks over and over. We can repeat them
with the function `loop`:

~~~haskell
> dac $ loop $ mel [htom, mtom, ltom, sn]
~~~

##### Time to make a pause

We can make a sample that contains a silence and lasts for certain amount of beats
with function `rest`:

~~~haskell
rest :: D -> Sam
~~~

Let's silence out some hi-hats:

~~~haskell
> let hhats = loop $ mel [lim 8 $ pat' [1, 0.5, 0.25, 0.1] [1] chh, rest 8] 

> dac hhats
~~~

It's interesting to note how we can assemble the whole musical composition out
of simple parts. The program is a sequence of applications of functions to values
we don't have special instrument and score sections. This brings a great flexibility 
to the whole process.


##### Transformation of audio signals

We can transform audio entities with `at` and `mixAt` functions. Simplified (conceptual) signature:

~~~haskell
at :: Audio a => (Sig -> Sig) -> a -> a
~~~

So it applies a signal transformation function to some value that contains signal.
It's rather simplified signature. The actual function `at` can also apply functions
with side effects `Sig -> SE Sig` or functions that take in mono signals and produce stereo signals.
And it transforms the second argument to the correct result. 

There is also a function `mixAt`:

~~~haskell
mixAt :: Audio a => Sig -> (Sig -> Sig) -> a -> a
~~~

It takes in a dry/wet ratio (0 to 1) as the first argument. Let's add a bit of reverb to the drums:

~~~haskell
> dac $ mixAt 0.2 smallRoom2 drums
~~~

##### Trick: filtering with LFO

Let's make our hi-hats a bit more alive. We are going to add filtering with center frequency modulated with low frequency oscillator (LFO):

~~~haskell
> let filteredHats = mul 4 $ at (mlp (500 + 4500 * uosc 0.1) 0.15) hhats

> dac filteredHats
~~~

The new functions:

Moog low-pass filter (alias for csound moogvcf)

~~~haskell
mlp :: Sig -> Sig -> Sig -> Sig
mlp centerFrequency resonance asig = ...
~~~

Unipolar pure sine wave:

~~~haskell
uosc :: Sig -> Sig
uosc frequency = ...
~~~

### Let's mix drums with drone

Recall that we had the value `drone` of the type `SE Sig2`
ad now we have the value drums of the type `Sam`. Can we play them together?
To do it we need to bring them to the common type. Then we
can just sum them up. 

There is a function that wraps a signal-like values to samples:

~~~haskell
toSam  :: ToSam a => a -> Sam           -- infinite
limSam :: ToSam a => D -> a -> Sam      -- finite
~~~

The expression `ToSam a => ` in the signature means that input
can be any value `a` that supports a set of functions from the interface `ToSam`.
The `toSam` creates an infinite sample from the signal the `limSam` 
creates finite samples with given number of beats in fact it's just a combo
of `lim` and `toSam` functions.

So with function toSam we can convert the `drone` to sample. Let's mix it all:

~~~haskell
> let drone = toSam $ mul 0.6 $ mean $ fmap (testDrone2 . cpspch) [7.02, 7.09, 8.02, 8.06]

> let drums = sum [...]

> let player = toSam $ atMidiTemp young1 harpsichord

> let performance = sum [mul 0.74 drone, mul 1.2 drums, mul 0.5 player]

> vdac performance
~~~

We can use just `dac` in place of `vdac` if we have the real midi-device attached to our computer.

### Let's record our performance live

We can record our song live with function:

~~~haskell
dumpWav :: String -> (Sig, Sig) -> SE (Sig, Sig)
~~~

The function dumps the audio to file and sends it through to the next audio unit.
It's useful for testing. We can use as many `dumpWav` functions in our code as we like.
This way for example we can record our performance by instruments. But now we are going
to record the whole performance.

Let's apply it:

~~~haskell
> vdac $ at (dumpWav "song2.wav") performance
~~~

and we can play it back right in the interpreter:

~~~haskell
> dac $ loopWav 1 "song2.wav"
~~~

The `loopWav` is an alias for `diskin2` opcode.

Let's play in reverse:

~~~haskell
> dac $ loopWav (-1) "song2.wav"
~~~

Let's go nuts:

~~~haskell
> dac $ loopWav (constSeq [1, 1, -1, 2, 1] 1) "song2.wav"
~~~

Th function `constSeq` is a simple step sequencer. It accepts a list of values
and repeats them with the given rate. For example we can create simple arpeggiators
with it:

~~~haskell
> dac $ tri (constSeq [220, 330, 440] 6)
~~~

Also we can add a bit of reverb:

~~~haskell
> dac $ mul 0.25 $ mixAt 0.17 largeHall2 $ tri (constSeq [220, 330, 440] 6)
~~~

The library csound-expression is based on signals. The audio components take in signals
and produce signals even application of an instrument to scores produces a signal.
With this model it becomes very easy to apply an effect like reverb. We just apply the
function to the signal that contains the mix of the whole song. In this sense
the signals in the CE are not just streams of numbers. They can contain more
involved data structures that can be rendered to Csound signals in the end.
This direct routing (with application of functions) can save us from using
the global variables or routing of mixed signals as it happens in Csound.

#### Reusing the recorded audio

We can incorporate our audio file into performance:

~~~haskell
vdac $ sum [ 
  cfd (usqr 0.25)  
    (toSam (loopWav (-1) "song2.wav")) 
    drums, 
  mul 0.5 player]
~~~

The crossfade:

~~~haskell
cfd :: SigSpace a => Sig -> a -> a -> a
~~~

It can crossfade between values of many types not just signals.

The unipolar square wave to switch between drums and recorded audio:

~~~haskell
usqr :: Sig -> Sig
~~~

note that there is  more simple way to load the audio files to samples.
We can use the functions

~~~haskell
wav1 :: String -> Sam

wav  :: String -> Sam
~~~

The `wav1` is for mono audio files and the `wav` is for stereo ones.
The `wavr` and `wavr1` play files in reverse.

Also we can convert the samples to signals. There is a function that
renders the samples:

~~~haskell
runSam :: D -> Sam -> SE Sig2
~~~ 

The first argument is BPM.


### Let's record offline

We have recorded the audio with function `dumpWav` it sends the audio
through and dumps it to disk. It's good to record the live performance. 
But often we want to record predefined music. The music that can be played solely by computer
without our intervention. In this case we can save a lot off time if we  can
record the music off-line. The Csound can often render the audio much faster 
then real time. Also this mode is useful the other way around. When the audio
is so complicated that it can not be played in real time but we can record
it off-line. 

To record offline we need to substitute the `dac` function with function `writeSnd`
since we don't want to send the audio to speakers:

~~~haskell
writeSnd :: String -> Sig2 -> IO ()
~~~

We can use it like this:

~~~haskell
> writeSnd "drums2.wav" $ fmap (setDur 60) $ runSam (120 * 4) drums
~~~

With `setDur` we set the duration in seconds of the signal to record.

We can play it back:

~~~haskell
> dac $ loopWav 1 "drums2.wav"
~~~

### Using UIs

The Csound has built in support for UI-widgets (they are implemented with FLTK). 
There is support for UI in Csound expression also. But it's organized in different way.

In the Haskell library UI is a container for the value augmented with visual appearance.
We can combine containers together to create a compound value. We can apply functions to
them store them in data structures and so on. 

Let's look at the function that creates knob. The knob produces unipolar control signal (from 0 to 1):

~~~haskell
uknob :: D -> Source Sig
uknob initValue
~~~

It takes in an initial value. The output is wrapped in the type `Source`. The source 
ties together value and appearance. 

We can apply  function within that container with the help of `lift1`:

~~~haskell
lift1 :: (a -> b) -> Source a -> Source b
~~~

The `(a -> b)` is a function from `a`'s to `b`'s. The output is also wrapped in the container `Source`
but the output is processed with the function. For example let the knob be the volume controller. We can map the volume
value to audio signal like this:

~~~haskell
> let synt vol = mul vol (osc 440)

> dac $ lift1 synt $ uknob 0.5
~~~

Notice that with let we can define not only constants but also functions. Our function `synt`
takes in volume as an argument.

There is another type of knobs. It's useful for frequencies. It produces
exponential values in the given range:

~~~haskell
type Range a = (a, a)

xknob :: Range Double -> Double -> Source Sig
~~~

Let's create a knob that controls a frequency of our synt:

~~~haskell
> let synt cps = tri cps

> dac $ mul 0.5 $ lift1 synt $ xknob (110, 1000) 220
~~~

We can combine the two examples with functions `hlift2`  and `vlift2`:

~~~haskell
hlift2, vlift2 :: (a -> b -> c) -> Source a -> Source b -> Source c
~~~

They apply the function of two arguments to two values made with widgets
and stack the visuals `h`orizontally `v`ertically.

Let's see how it works:

~~~haskell
> let synt amp cps = mul amp (tri cps)

> dac $ hlift2 synt (uknob 0.5) (xknob (110, 1000) 220)
~~~

Try to change `hlift2` with `vlilft2` and see what happens.
The interesting thing about this program is how we can 
create the whole audio synthesizer with knobs by a single line of code.

Also there are `hlift` and `vlift` functions for functions of three and four arguments.
There are functions that even take in lists of widgets:

~~~haskell
hlifts, vlifts :: ([a] -> b) -> Source [a] -> Source b
~~~

We can create a simple mixing console for our example. We have our individual parts:

~~~haskell
let drone = ...
let drums  = ...
let player = ...
~~~

Let's create a mixer function:

~~~haskell
> let mixing [total, v1, v2, v3] = mul total $ sum $ 
        zipWith mul [v1, v2, v3] [drone, drums, player]
~~~

You should write it in the single line of code in the interpreter. I've divided it in two lines for readability.
The function `zipWith` maps over two lists. It applies a function
of to arguments to the individual components of two lists:

~~~haskell
zipWith f [a1, a2, a3] [b1, b2, b3]  ===  [f a1 b1, f a2 b2, f a3 b3]
~~~

We can create four knobs to control the volumes:

~~~haskell
> dac $ hlifts mixing $ fmap uknob [0.7, 0.7, 1, 0.4]
~~~

There are other widgets like sliders, check boxes, buttons. The interested reader
should study the documentation for the library on [github](https://github.com/spell-music/csound-expression).

### Beyond interpreter

So far we made all programs within the interpreter. It's useful
for making sketches and quick testing of ideas but sometimes 
we want to save our ideas to reuse them.
We need to be able to write Haskell modules and compile and load them
to the interpreter. Here is the simplest possible program:

~~~haskell
module Synt where

import Csound.Base

main = dac $ osc 220
~~~

The `Synt` is the name of the module. we should save it to the module `Synt.hs`.
The value `main` is an entry point for a program. Runtime system starts to execute
the program from the function main. 

We can compile and run the program by executing in the system command line:

~~~haskell
runhaskell Synt.hs
~~~

Also we can define modules without function `main`. Then our module
defines a set of values to be used in the interpreter or inside another module.

We can load the module by passing it as an argument to the ghci at start up:

~~~haskell
ghci Synt.hs
~~~

Or after entering the ghci we can load the module with the command `:l` (short for load):

~~~haskell
> :l Synt.hs
~~~

If we have made the changes in the module we can reload it with command `:r` (short for reload):

~~~haskell
> :r
~~~

I like to experiment in the interpreter then I save the parts I like to some module, reload it
to the interpreter and start to build the next values on top of the things I've defined before.


## II. Case study: Vibhu vibes

As the last example I'd like to share the process of creation of the real track.
It's called vibhu vibes. You can listen to it on the [soundcloud](https://soundcloud.com/anton-kho/vibhu-vibes).

Here is the complete code for the piece:

~~~haskell
import Csound.Base
import Csound.Patch

main = vdac $ sum [ synt, return $ mul 1.5 glitchy ]

glitchy = mixAt 0.2 smallRoom2 $ 
    mul (sqrSeq [1, 0.5, 0.25] 8) $ 
        sum [ loopWav1 (-(constSeq [1, 2, 4, 2] 0.5)) file
            , mul (constSeq [1, 0] 0.5) $ loopWav1 (-0.25) file]

synt = sum 
    [ atMidi $ vibhuAvatara 65 (uosc 0.25)
    , mul pulsar $ atMidi $ prakriti 34
    , atMidi $ mul (0.5 * uosc 0.25) $ whaleSongPad ]
    where 
      pulsar = sawSeq [1, 0.5, 0.25, 0.8, 0.4, 0.1, 0.8, 0.5] 8

file = "loop.wav"
~~~

It was improvised live and recorded with `dumpWav` function.
Here I use vdac for tutorial purpose but the `dac` function with
real midi-device was used. 

You can write the whole program in the interpreter in the single
but rather long line o code. It's not the praise or the benefit of writing 
everything in one line its more for the compositional nature
of the  model for computer music creation.

Let's break this file apart. The music has only two parts. 
Thy are drum part and synt part. The drum part is created 
by playing back the ordinary drum loop at strange rates.
Here I use my own file "loop.wav" But you can insert any
short drum loop that you like or download the file at the [repo on github](https://github.com/anton-k/talks/tree/master/HaL/audio). 
The synt part is created 
with three pads that are playing at the same time. 
So it's a layered synthesizer. 

Let's take a closer look at the drum part.

### Glitch: Pulsating noise

The main idea of the drum part can be illustrated with pink noise:

~~~haskell
> dac $ mul (sqrSeq [1, 0.5, 0.25] 8) $ pink
~~~

The `sqrSeq`  is just like `constSeq`. It's a step sequencer. 
The only difference is that each step is created with unipolar square wave shape.
In the case of `constSeq` it is just a constant value.

We create rhythmical bursts. But can we substitute the pink noise
with something more interesting?

### Glitch: Let's try drum file weird playbacks

Let's play some short drum loop:

~~~haskell
> let file = "/home/anton/loop.wav"
> dac $ loopWav1 1 file
~~~

Let's try in reverse:

~~~haskell
> dac $ loopWav1 (-1) file
~~~

Maybe different speeds:

~~~haskell
> dac $ loopWav1 0.5 file
> dac $ loopWav1 (-0.25) file
~~~

Let's mess around with changing speed:

~~~haskell
> dac $ loopWav1 (-(constSeq [1, 2, 4, 2] 0.5)) file
~~~

We can also alter amplitude:

~~~haskell
> dac $ mul (constSeq [1, 0] 0.5) $ loopWav1 (-0.25) file
~~~

So here is the basis for our drum pulsating noise:

~~~haskell
let d1 = loopWav1 (-(constSeq [1, 2, 4, 2] 0.5)) file

let d2 = mul (constSeq [1, 0] 0.5) $ loopWav1 (-0.25) file

let noisyDrum = sum [d1, d2]

dac noisyDrum
~~~

### Glitch: Adding pulsar and reverb

We can add a reverb and pulsar from the pink noise example:

~~~haskell
let glitchy = mixAt 0.2 smallRoom2 $ mul (sqrSeq [1, 0.5, 0.25] 8) noisyDrum

dac glitchy
~~~

That's our final glitch for the track. Let's create an interesting pad synthesizer.

### Drone

The main idea for the drone is to mix several cool pads from standard collection 
and add a pulsar synchronized with the beat to one of the pads.

Let's try a couple of spacious pads:

~~~haskell
> vdac $ mul 0.5 $ atMidi nightPad

> vdac $ mul 0.5 $ atMidi $ deepPad nightPad
~~~

The `deepPad` is an interesting function it takes in a patch and creates new
patch where every played note is accompanied with the note of the same pitch but octave below.
Can you think of how it can be implemented in Csound.
We can substitute the nighPad with some other pads like: `fmDroneMedium`, `pwPad`, `dreamPad`, `whaleSongPad`.

### PADSynth pads

If we have Csound 6.05 or higher we can try out nice pads based on PADSynth algorithm:

~~~haskell
> vdac $ mul 0.45 $ atMidi $ vibhu 45

> vdac $ mul 0.45 $ atMidi $ prakriti 45

> vdac $ mul 0.45 $ atMidi $ avatara 45
~~~

The argument for the function ranges from (1 to 100 or even higher).
It controls the thickness of the bands. With higher values
e can get more chorused instruments.

There are pads that can crossfade between those pads:

~~~haskell
> vdac $ mul 0.45 $ atMidi $ vibhuAvatara 65 (uosc 0.25)
~~~

### Mixing pads

We can experiment to find the right mixture of the PADs

~~~haskell
> vdac $ mul 0.3 $ sum [atMidi dreamPad, atMidi $ deepPad fmDroneMedium]

> vdac $ mul 0.3 $ sum [atMidi pwPad, atMidi $ deepPad whaleSongPad]
~~~

### Adding pulsation

We can add another pad and multiply it's output with rhythmic pulsating envelope:

~~~haskell
> let pulsar = sawSeq [1, 0.5, 0.25, 0.8, 0.4, 0.1, 0.8, 0.5] 8

> vdac $ mul pulsar $ atMidi nightPad
~~~

### Final drone

Let's try them together:

~~~haskell
> let p1 = atMidi whaleSongPad

> let p2 = atMidi $ deepPad overtonePad

> let p3 = mul pulsar $ atMidi nightPad

> let pads = mul 0.3  $ sum [p1, p2, p3]

> vdac pads
~~~

Let's put together drums and drone:

~~~haskell
> vdac $ sum [pads, return glitchy]
~~~

## III. Conclusion

I hope that you have enjoyed the journey. It's hard to fit all the features
of the library into a single article. I've tried to choose the most interesting
and easy to use components. But many features are left out like creation
of scores and event streams, functions for advanced synthesis techniques like
granular synthesis. You can read about them in the guide at the [github page
of the project](https://github.com/spell-music/csound-expression).

The main idea of the library is the motto from the SICP book which is actually based on Scheme that

    everything is an expression

Everything can be combined  by applying the functions to values. There is no 
special syntax beyond this simple idea. This can greatly enhance the productivity 
of the Csound user. Also the Haskell gives the user ability to package things into the libraries
and easily redistribute your synthesizers. You can create a package of your own 
patches and workflows for performances or download someone else's modules.
No need for include macroses. It just has the normal module system. 

There are certain limitations of the library. Some features are not implemented.
Right now we can not use arrays, the while statement doesn't 
work properly. There are some known bugs. Not many of them but they are present. 
But it can change in the future. Nonetheless the library is pretty stable and usable.
You can listen to some music that was made with it on [soundcloud](https://soundcloud.com/anton-kho).

Further links:

* Guides for the library: 

    * [csound-expression](https://github.com/spell-music/csound-expression)

    * [csound-sampler](https://github.com/spell-music/csound-sampler)

* Hackage documentation:

    * [csound-expression](https://hackage.haskell.org/package/csound-expression)

    * [csound-sampler](https://hackage.haskell.org/package/csound-sampler)

    * [csound-catalog](https://hackage.haskell.org/package/csound-catalog)

* Learn haskell books, all of them are available for free online: 

    * [Learn you a haskell for a great good](http://learnyouahaskell.com/)

    * [Real world Haskell](http://book.realworldhaskell.org/read/)

    * [Yet another haskell tutorial](https://www.umiacs.umd.edu/~hal/docs/daume02yaht.pdf) (it's rather old but good one).

* Monad tutorials:

    * [Dan Pipponi, You could have invented monads](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)

    * My tutorial, [Monads for drummers](https://github.com/anton-k/monads-for-drummers)

*Happy Csounding and happy Haskelling!* 

## IV. Reference

Some types and functions for quick start.

### Basic types

In the library we have just several basic types:

~~~haskell
Sig  -- audio and control signals

D    -- constant numbers

Tab  -- functional tables

SE   -- Side-effects

Spec -- spectrums (used in pvs opcodes)
~~~

### Rendering the audio

~~~haskell
dac   -- send audio to speakers

dacBy -- supply options (rates, drivers, midi-devices)

vdac  -- dac with virtual midi-keyboard.

writeSnd -- render audio to file offline

writeSndBy -- supply options (rates, drivers, midi-devices)

setRates  -- sets the sample rate and the block size

setJack   -- sets the jack name
~~~

Examples:

~~~haskell
> let opt = setRates 48000 128 <> setJack "sine-wave"
> dacBy opt (osc 220)
~~~

We use operator `<>` to combine the options. See the standard class [Data.Monoid](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html) for more information.


### Sound design tools

#### Audio waves

Pure sine, sawtooth, square, triangle, pulse width:

~~~haskell
osc, saw, sqr, tri :: Sig -> Sig

pw :: Sig -> Sig -> Sig
pw bandwidth frequency = ...
~~~

Unipolar waves (useful for LFOs): `uosc`, `usaw`, `usqr`, `utri`.

Examples:

~~~haskell
> dac $ mul 0.5 $ tri $ 220 * (1 + 0.08 * uosc 3)

> dac $ mul 0.25 $ pw (0.5 * uosc 0.12) 220 + pw (0.2 + 0.3 * uosc 0.2) 220
~~~

#### Envelope generators

~~~haskell
linseg, expseg :: [D] -> Sig
~~~

Just like in Csound but arguments are passed in  the list and the last value is held:

~~~haskell
> linseg [0, 0.2, 1, 1.3, 0.5, 1.5, 0]
~~~

So the zero is held it's not going to drop down to infinity.

**L**inear adsr and e**x**ponential adsr **e**nvelope **g**enerators:

~~~haskell
leg, xeg :: D -> D -> D -> D -> Sig
~~~

Attack-sustain-release envelope:

~~~haskell
fades :: D -> D -> Sig
fades fadeInTime fadeOutTime = ...
~~~

Examples:

~~~haskell
> dac $ osc $ 220 * (1 + 0.5 * linseg [0, 2, 1, 2, 0.5, 1, 0.5, 1, 0])

> let env = leg 0.02 0.1 0 0

> dac $ mul env $ sqr $ 220 * env

> vdac $ midi $ onMsg $ mul (fades 0.1 0.5) . osc
~~~

#### Filters

Moog-like low pass filter:

~~~haskell
mlp :: Sig -> Sig -> Sig -> Sig
mlp centerFreq resonance asig = aout
~~~

Notice that the order of arguments is reversed. It's not like in Csound.
The reason for that is that in Haskell it's convenient to use less used
arguments as first arguments. Because in Haskell we have partial application. 
With partial application if we apply single argument to the function of to arguments
it doesn't lead to type error. It creates a function of one argument. The first
argument is bound to a passed value and the second is free to use.

Here is an example:

~~~haskell
> :t lp
mlp :: Sig -> Sig -> Sig -> Sig

> :t (lp 1500)
(mlp 1500) :: Sig -> Sig -> Sig

> :t (mlp 1500 0.4)
(mlp 1500 0.4) :: Sig -> Sig

> :t (mlp 1500 0.4 $ saw 200)
(mlp 1500 0.4 $ saw 200) :: Sig
~~~

We gradually reduce the number of arguments in the expression by
passing more arguments to the function `mlp`. The order of arguments is the same for other filters

Ordinary filters, low, high, band pass and band reject filters:

~~~haskell
lp, hp, bp, br :: Sig -> Sig -> Sig -> Sig
~~~

Add z ass prefix to get zero-delay filters:

~~~haskell
zlp, zhp, zbp, zbr :: Sig -> Sig -> Sig -> Sig
~~~

Ladder filters (moog-like and zero delay):

~~~haskell
ladder, zladder :: Sig -> Sig -> Sig -> Sig
~~~

Butterworth filters:

~~~haskell
blp, bhp :: Sig -> Sig -> Sig

blp centerFreq ain = aout

bbp, bbr :: Sig -> Sig -> Sig -> Sig

bbp centerFreq reson ain = aout
~~~

Examples:

~~~haskell
> dac $ mlp (3500 * uosc 1) 0.1 $ saw 220

> dac $ mlp (3500 * uosc (linseg [1, 2, 4, 1, 2, 0.5, 8, 0.5, 2, 4, 0])) 0.1 $ saw 220
~~~

#### Creation of functional tables

Play oscillator with given table:

~~~haskell
oscBy :: Tab -> Sig -> Sig
~~~

Harmonic series

~~~haskell
sines :: [Double] -> Tab
~~~

Harmonic series with exact frequencies:

~~~haskell
type PartialNumber = Double
type PartialStrength = Double

sines2 :: [(PartialNumber, PartialStrength)] -> Tab
~~~

Linear and exponential curves:

~~~haskell
lins, exps :: [Double] -> Tab
~~~

Set table size and add guarding point:

~~~haskell
setSize :: Int -> Tab -> Tab
guardPoint :: Tab -> Tab
~~~

Skip normalization:

~~~haskell
skipNorm :: Tab -> Tab
~~~

Examples

~~~haskell
> dac $ mul (uosc 0.5 * usqr 4) $ oscBy (sines [1, 0.5, 0, 0, 0.25]) 220
~~~

#### Midi

Creates audio signal out instrument definition and user midi input.

~~~haskell
midi :: Sigs a => (Msg -> SE a) -> SE a
~~~

The `Msg` is the midi message we can read amplitude and frequency with ampCps function:

~~~haskell
ampCps :: Msg -> (D, D)
~~~

Useful function `onMsg`. It converts function that takes in a frequency signal or constant
or pair of amplitude and frequency to the function that is defined on messages. It's often
goes hand at hand with function `midi`:

~~~haskell
> vdac $ midi $ onMsg osc
~~~

We can add envelope to remove clicks and pops:

~~~haskell
> let synt cps = mul (fades 0.01 0.5) $ osc cps

> vdac $ mul 0.5 $ midi $ onMsg synt
~~~


#### Reverbs

Reverbs: smallRoom2, smallHall2, largeHall2, magicCave2:

~~~haskell
> let x = mul (uosc 0.5 * usqr 4) $ oscBy (sines [1, 0.5, 0, 0, 0.25]) 220

> dac $ mixAt 0.25 largeHall2 x

> let synt = midi $ onMsg $ mul (fades 0.01 0.7) . tri

> vdac $ mul 0.25 $ mixAt 0.25 magicCave2 synt
~~~

#### Delays

~~~haskell
type MaxDelayTime = D
type Feedback = Sig
type Balance = Sig

echo :: MaxDelayTime -> Feedback -> Sig -> SE Sig
pingPong :: DelayTime -> Feedback -> Balance -> Sig2 -> SE Sig2
~~~

Example:

~~~haskell
> let synt = midi $ onMsg $ mul (fades 0.01 0.7) . tri

> vdac $ mul 0.25 $ mixAt 0.25 largeHall2 $ mixAt 0.65 (echo 0.5 0.8) synt
~~~

### Magic functions

There are certain magic functions that are defined on arguments of many types.

#### Volume control

Scales the amplitude of something that produces signals. It can be a single signal
or tuples of signals or signals wrapped in the `SE` or produced with UI-widget.

~~~haskell
mul :: Audible a => Sig -> a -> a
~~~

#### Transformation of signals

The `at` converts something audible with signal-like function
and `mixAt` converts with dry-wet ratio. It's the first argument that ranges from 0 (all dry)
to 1 (all wet).

~~~haskell
at    :: Audible a => (Sig -> Sig) -> a -> a

mixAt :: Audible a => Sig -> (Sig -> Sig) -> a -> a
~~~

### Patches

Plays patch with midi:

~~~haskell
atMidi :: Sigs a => Patch a -> SE a
~~~

Plays single note:

~~~haskell
atNote :: Sigs a => Patch a -> (D, D) -> SE a
atNote patch (amplitude, frequency) = ...
~~~

</div>