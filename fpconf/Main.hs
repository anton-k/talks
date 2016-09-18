module Tam where

import Csound.Base
import Csound.Sam
import Csound.Patch

bpm:: Fractional a => a
bpm = 85

t :: Fractional a => a
t = bpm / 60

run = dac . runSam bpm
run' = runSam bpm

sf x = "samples/" ++ x

drum82 n x = ramLoop (-2) (n * bpm / 82) 1 (sf ("drum-82/" ++ x))
drum88 n x = ramLoop (-2) (n * bpm / 88) 1 (sf ("drum-88/" ++ x))

bass a = drum82 a "bass.wav"
bd   a = drum82 a "bass drum.wav"
hh   a = drum82 a "hihat.wav"
perc a = drum82 a "perc.wav"
sit  a = drum82 a "sitars.wav"
sn   a = drum82 a "snare.wav"
sme  a = drum88 a "synth melody.wav"


{-
syncChange :: (SigSpace a) => Sig -> a -> a
syncChange bpm asig = mapSig (\x -> samphold x (metroSig (bpm / 60))) asig

symCharGroup :: (SigSpace a, Sigs a) => Sig -> Sig -> [(Char, a)] -> String -> a
symCharGroup v0 bpm xs str = syncChange bpm $ charGroup v0 xs str


-}
symCharGroup bpm v0 xs str = syncCharGroup bpm (Just v0) xs str

chars v0 k ks xs = symCharGroup (bpm / k) v0 (zip ks xs) ""


col1 = ['1', 'q', 'a', 'z']
col2 = ['2', 'w', 's', 'x']
col3 = ['3', 'e', 'd', 'c']
col4 = ['4', 'r', 'f', 'v']
col5 = ['5', 't', 'g', 'b']
col6 = ['6', 'y', 'h', 'n']
col7 = ['7', 'u', 'j', 'm']
col8 = ['8', 'i', 'k', ',']
col9 = ['9', 'o', 'l', '.']
col0 = ['0', 'p', ';', '/']


ctrlSme = chars 0 1 col1 [1, -1, -2, 0]
ctrlSme2 = chars 0 2 col2 [1, -1, 2, 0]
ctrlSit = chars 0 2 col3 [1, -1, -2, 0]
ctrlHh  = chars 0 1 col4 [1, 2, 4, 0]
ctrlPerc  = chars 0 4 col5 [1, -1, 2, 0]
ctrlBd  = chars 0 8 col6 [1, -1, 2, 0]
ctrlSn  = chars 0 8 col7 [1, -1, 2, 0]

stutSme, stutSme2, stutSit :: Sig

stutSme  = charToggle def 'i' 1
stutSme2 = charToggle def 'o' 1
stutSit  = charToggle def 'p' 1

stut :: [Sig] -> Sig -> Sam -> Sam
stut xs ctrl a = cfd ctrl a (mul (2.2 * sqrSeq xs (4 * t)) a)

stSme  = stut [1, 0.5, 0.25, 1, 0.5, 0.25, 0.8, 0.4] stutSme
stSme2 = stut [1, 0.5, 0.25] stutSme2
stSit  = stut [1, 0.5, 0.25, 0.1] stutSit

main = run $ mul 0.6 $ har 
		[ bass 	1
		, bd	ctrlBd
		, mul 2.5 $ hh 	ctrlHh
		, perc 	ctrlPerc
		, sn 	ctrlSn
		, mul 0.7 $ stSme $ sme   ctrlSme
		, mul 0.6 $ stSme2 $ sme   ctrlSme2
		, stSit $ sit   ctrlSit

		, mul 0.95 vib1
		]
	

vib1 = toSam $ atMidi caveOvertonePad
