# Tableau Flow Maps

A simple visualizer for your Tableau dashboard actions relationships and how they impact the data flow.

Tableau's dashboard "action" system, when combined with parameters, calculated fields, and filter inputs allows for some very complex interactions to be orchestrated. The problem is that Tableau really doesn't give you a very good way to holistically look at all those UX relationships and how they interact together.

Enter Flow Maps - a simple viewer that attempts to fill this gap - even if only for a knowledge transfer / documentaion sake.

## Flow Maps is trying to answer these kind of questions:

### When a user interacts with a (source) Worksheet
- What fields are involved?
- What other worksheets does this target
- What action (or actions) is responsible?

### When a user triggers a Set Parameter action
- What are the downstream worksheets affected by this (since they use the param in question - directly or indirectly)

### When a user interacts with a filter widget
- What worksheets 

### And more abstractly: Do any of these interactions clash?


![teaser-image3](https://github.com/ryrobes/tabflowmaps/blob/master/resources/public/images/ghthumb-double.png?raw=true)

Try it out live at http://tabflowmaps.com/ 

![teaser-image2](https://github.com/ryrobes/tabflowmaps/blob/master/resources/public/images/tfm-logo.png?raw=true)

## Backend server required also

https://github.com/ryrobes/tabflowmapssvc 

## To Compile

TODO

...

## To Develop

This app is in 2 parts, one small backend service to convert the XML into EDN and host the front-end web code - and the front-end web code itself (this repo). TODO

...

## License

Copyright © 2023 Ryan Robitaille

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.