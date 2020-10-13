How mixing works
----------------

The file [softmix.s](softmix.s) is a home for sound mixing routines. It is written in MC68020+ assembler and could not be compiled with AmigaE because of the usage of an extended instruction set.

32 voice channels are allocated for the simultaneously played samples.

The mixing of each voice is performed on 24-bit samples in a 32-bit buffer, separate for each stereo channel. 16-bit samples are premultiplied by each stereo channel's volume (256 = 100%). The volume is a result of several factors, such as the envelope, bank volume, velocity, panorama, etc. The result is being converted to signed 16-bit samples with [clamping][clamped].

```
76543210 76543210 76543210 76543210
EEEEEEEE SSSSSSSS SSSSSSSS FFFFFFFF

E - Bits exceeding sample domain; contribute to clamping.
S - Output 16-bit signed integer.
F - Fraction bits only contribute when mixing samples.
```

There are several mixing routines. Different routines are being called depending on the sample type (mono/stereo), if the sample is being played forward or backwards, but also which voice is being mixed in.

* [1st voice] - samples are stored in the mixing buffer.
* [2nd voice] and following voices except the last one - samples are added to the mixing buffer.
* The [last voice] - samples are added from the mixing buffer and converted to a [clamped] 16-bits signed integers.

* `firstloop`/`secloop`/`lastloop` - stereo samples forward: 1st/nth/last.
* `firstloopm`/`secloopm`/`lastloopm` - mono samples forward: 1st/nth/last.
* `firstloopbk`/`secloopbk`/`lastloopbk` - stereo samples backwards: 1st/nth/last.
* `firstloopmbk`/`secloopmbk`/`lastloopmbk` - mono samples backwards: 1st/nth/last.

[clamped]: https://github.com/royaltm/Amiga-midiIn/blob/4707f0fae7c563cd32e1a7c065396bbc8fbb862d/src/modules/softmix.s#L506
[1st voice]: https://github.com/royaltm/Amiga-midiIn/blob/4707f0fae7c563cd32e1a7c065396bbc8fbb862d/src/modules/softmix.s#L470
[2nd voice]: https://github.com/royaltm/Amiga-midiIn/blob/4707f0fae7c563cd32e1a7c065396bbc8fbb862d/src/modules/softmix.s#L556
[last voice]: https://github.com/royaltm/Amiga-midiIn/blob/4707f0fae7c563cd32e1a7c065396bbc8fbb862d/src/modules/softmix.s#L565