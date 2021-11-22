Cirdis - Circuit Dissector - PCB Reverse Engineering Tool
=========================================================

Cirdis aims to make reverse engineering 2-sided PCB's succeed in a breeze. It allows you to load pre-aligned images
and trace out the PCB features in a way that is extremely quick and fun compared to using an image editor or pen and paper.

Cirdis is meant to be used on a desktop/laptop with a keyboard and mouse. It can also be used with a mouse only and while 
it works on mobile devices, no effort has been made to optimize the experience for touch screens.

Contribution Policy
---
Cirdis is open source but the repository is <ins>**closed to unrequested outside code contributions**</ins>. For the
time being I prefer to maintain both full copyright in this repository and also a solid grasp on the feature scope.

Feature requests/ideas/improvements can be submitted via issues. Please provide sufficient detail in describing the functionality and
why it would be important to include in this software. The scope of the feature should also be reasonable.

Bugs must describe the steps to reproduce, the expected outcome and the actual outcome.

How to use
----

1. Align layer(s) using your preferred image editor
2. Import layer(s)
3. Place THT pads and vias
4. Place SMT pads
5. Draw traces
6. Bob's your uncle

<h5>Other notes</h5>

* Middle mouse pans
* Mouse wheel zooms
* shift+wheel changes pad size and trace thickness

Features
---

<h5>Implemented</h5>

* Import any number of layers
* THT pads (through hole)
* SMT pads (surface mount)
* Surface traces
* Snapping
* Undo/redo
* Keyboard + Mouse optimized UI
* Component utilities (pin numbering, fast pin placement)

* <h5>Planned</h5>

* Dimensioning (define a known distance and measure stuff)
* Snap to angle
* Prevent overlapping component placement
* Deleting stuff
* Snap to trace segment
* Custom nets (name + color)
* Pin labels
* Save/Load project


<h5>Exploration</h5>

* undo/redo with multiple timelines (branching)

Roadmap
----

Primary milestone is to support dissecting normal 1 or 2-sided PCB's. If there is demand, the tool could possibly be extended
to support proper multi-layer boards (needs support for blind vias) or even silicon reverse engineering but these are currently
outside of the scope. Feel free to discuss.

Other software
---

There have been some attempts

* https://github.com/pcbre/pcbre (Python, active development but no new releases in a long time)
* https://github.com/unixdj/depcb (C, stale)
* https://github.com/MartijnBraam/pcb-re-toolkit (vanilla JS, limited features, stale)
* https://github.com/HenniePeters/RevEngE (C++, active, minimal features)
* https://github.com/tom-2015/unPCB (Visual Basic, stale)


