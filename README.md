midiIn
======

Hereby I humbly present sources of the [MIDI-controlled sample player] for AMIGA under the [GPL-3.0-or-later](COPYING) license. It's Open Source now.

History
-------

`midiIn` was developed by me from early 1995 until late 1999 and it was my first mature project. The idea came from Sebastian Witkowski, who was also a big fan of the AMIGA, and as a professional musician, he wished he could play samples without pesky limits, using the MIDI interface, in the studio, as well as on stage. There were many programs for AMIGA that provided similar functionality, but all of them was built primarily for other tasks and MIDI-controlled sample playing was always their secondary and minor functionality. `midiIn` was designed and developed solely with the only purpose in mind: to play as many samples as can fit in memory, pushing the AMIGA to her limits. Because of that, it became the best tool for such a task available at a time.

`midiIn` was battle-tested during many in the studio and on the [stage hours](https://www.youtube.com/watch?v=fZkUvcbqcgg&list=PL23E3B9D150F8F020) during [Najakotiva performances](https://www.youtube.com/watch?v=cOjqpoVyNVs&lc=Ugw2RlhaID3FPzMIYZ14AaABAg). Together in tandem with the AMIGA A1200+030 the Atari ST was also used with its awesome Cubase sequencer.

The first publicly released version was [midiIn v1.0](https://aminet.net/package/mus/midi/midiIn) which could only play 2 samples at once using a simple and already well-known trick to make Paula produce two 14-bit audio channels instead of its 4 DMA channels with the 8-bit quality. But it could run even on A500. At the time of its release, I already had the Unlimited version of mixing routines in the alpha stage. For 2 years, the program was tested by Sebastian, as well as several other musicians around the world, before I decided to release its successor: [midiIn32](https://aminet.net/package/mus/midi/midiIn32). This time it could play as many 16-bit samples, mixed in real-time, as the CPU could allow - which was around 10 on A1200+030 and 32 on MC68040/060.

Recently, these awesome [Musicians](https://twitter.com/_Adoru_/status/997923877224370176) from [The Other Days](http://theotherdays.net/) have contacted me, because they have been using `midiIn32` as of today, for at least a few past years, with their own custom-built MiST-based-ultimate-play-every-chiptune-format-and-mix-in-real-time-DJ-machine, and wanted to add some new features. For the past several months we have been discussing how to approach this, but I have finally decided that it would be best if I'll just release the sources.


Copying
-------

    midiIn v32.014, midiIn v32.020b
    Copyright (C) 1995-1999 Rafal Michalski <royaltm75@gmail.com>

    midiIn is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    midiIn is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with midiIn.  If not, see <https://www.gnu.org/licenses/>.

The above copyright notice applies to all of the files in this [repository](https://github.com/royaltm/Amiga-midiIn).


Versions
--------

Tagged as [32.020] is a [never before](Dokumentacja/News) released version of `midiIn`, with awesome "new" features, and with its sound mixing routines targeting the [AHI] subsystem instead of native Paula chipset. This one was not as thoroughly tested as the previous versions, but here it is.

I've also added sources of a pre-release version, tagged [32.014] which includes the original Paula-only mixing routines. Just do `git checkout 32.014`. Unfortunately, I didn't preserve sources for the released `midiIn` v32.15b but 014 is the closest I could find. Back in the days, GIT wasn't a thing yet and as a self-taught programmer without the Internet, version control systems were not commonly used as they are today. I don't even know if there was such a solution for AMIGA anyway. So I've developed my system, which was archieving source files between releases, but only a few of those archived files survived all of the hard disc crashes that happened in the meantime.


Prerequisites
-------------

* The AMIGA (or an emulator).
* [AmigaE] compilator with all the modules, including EasyGUI.
* [EasyGUI] v3+ (not sure if the whole installation is needed).
* An MC68k assembler to compile modules/softmix.s
* Assuming your `AmigaE` installation resides in `Prog:E_v3.2a`, add to the `user-startup` the following:

```
;BEGIN E
Assign >NIL: E: Prog:E_v3.2a
Assign >NIL: EMODULES: E:Modules
Path E:bin/ ADD
;END E
```

`midiIn` GUI was written in [AmigaE] using the [EasyGUI] library. Mixing routines are written in 68k asm and require an assembler that understands MC68020+ instructions.

To run `midiIn` you'll also need some libraries (these were distributed with the original [midiIn] archive file):

* reqtools.library v38+
* midi.library

and last but not least: a working [AHI] sound driver.

System `mathieeedoubbas.library` is also being extensively used. For an optimal experience speed it up with [FastIEEE].


Compilation
-----------

I'm writing this from memory, requires verification:

* Compile [modules/softmix.s](modules/softmix.s) to `modules/softmix.o`
* Run `Amiga Shell` script: [make](src/make)

This should produce `midiIn` executable in `src`.


Contribution
------------

Because I didn't have yet nowadays the opportunity to test the compilation process myself, feel free to create an issue if you have any trouble with making it compile. I'll try to address it if I'll be able to.

These sources are old, and not maintained since the last century (hehe). They are also very seldom commented, as they were not intended for opening, and many things may seem to be odd. But if you feel like it, I'll gladly accept pull requests.


[midiIn]: http://aminet.net/mus/midi/midiIn32.lha
[MIDI-controlled sample player]: https://aminet.net/package/mus/midi/midiIn32
[AmigaE]: http://strlen.com/amiga-e/
[EasyGUI]: https://aminet.net/package/dev/e/EasyGUI_v33b3
[AHI]: https://aminet.net/search?query=AHI
[FastIEEE]: https://aminet.net/package/util/sys/FastIEEE
[32.014]: https://github.com/royaltm/Amiga-midiIn/tree/32.014
[32.020]: https://github.com/royaltm/Amiga-midiIn/tree/32.020
