module Aornota.Sweepstake2018.UI.Program.Markdown.Literals

let [<Literal>] SCORING_SYSTEM_MARKDOWN = """Each sweepstake team will consist of a **team/coach**, **1 goalkeeper** and **10 outfield players**.

(Although outfield players have been categorized as defenders, midfielders or forwards, you can choose any combination you like, e.g. if you want to go for eight defenders and two
midfielders - the no-longer-fashionable [except in Northern Ireland] 8-2-0 formation - please do.)

The **team/coach** will score (or lose) points for:
+ **winning** a match: **20** or **16** or **12** (see below)
+ **drawing** a match: **8** or **6** or **4** (see below)
+ a team player receiving a **yellow card**: **-1**
+ a team player receiving a **red card**: **-3**

(If a player receives a second yellow card in a match, the two yellow cards will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight"
red card, both cards will be scored.)

Where multiple possible scores are given above, the score will depend on whether the team and their opponents are in the top 16 seeds:
+ if the team **is** a top 16 seed but their opponents are **not**, the **lowest** score will apply
+ if the team is **not** a top 16 seed but their opponents **are**, the **highest** score will apply
+ if **both** teams are top 16 seeds - or if **neither** team is - the **middle** score will apply

The top 16 seeds are (in order): Russia; Germany; Brazil; Portugal; Argentina; Belgium; Poland; France; Spain; Peru; Switzerland; England; Colombia; Mexico; Uruguay; and Croatia.

The remaining teams are: Denmark; Iceland; Costa Rica; Sweden; Tunisia; Egypt; Senegal; Iran; Serbia; Nigeria; Australia; Japan; Morocco; Panama; South Korea; and Saudi Arabia.

(Note that Russia are only in the top 16 seeds because they are hosting the tournament; based on the October 2017 world rankings, they are the worst team participating.)

**All players** will score (or lose) points for:
+ being named **man-of-the-match**: **15**
+ _**scoring**_ a **goal** or a **penalty**: **12**
+ _**assisting**_ a **goal**: **3** (note that a goal cannot be assisted by the same player who scored the goal - unless they win a free kick and then score directly from it)
+ _**winning**_ a **penalty**: **3** (note that a penalty can be won by the same player who took the penalty)
+ _**missing**_ a **penalty**: **-6**
+ _**scoring**_ an **own goal**: **-6**
+ receiving a **yellow card**: **-2**
+ receiving a **red card**: **-6**

(A penalty will be considered as "missed" irrespective of whether the goalkeeper touched the ball. And again, if a player receives a second yellow card in a match, the two yellow cards
will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight" red card, both cards will be scored.)

In addition, **goalkeepers** will score points for:
+ keeping a **clean sheet**: **12**
+ _**saving**_ a **penalty**: **12**

Note that outfield players can also score "goalkeeper" points if they end up playing in goal. (It probably won't happen - but you never know...)

(If more than one goalkeeper features for a team in a match, the "clean sheet" points will be awarded to whichever goalkeeper played more "regulation" minutes; if they played the same
amount of minutes, the points will be shared. A penalty will only be considered as "saved" if the goalkeeper touched the ball.)

Information about assists and such will be nicked from <https://www.whoscored.com/>.

As always, points can only be scored for goals / penalties / assists / &c. during normal time and extra time. **Penalty shootouts do not contribute to the scoring** [except to the extent 
that they determine who wins the match] - well, unless a player manages to get booked or sent-off during the shootout. Stranger things have happened..."""

let [<Literal>] PAYOUTS_MARKDOWN = """**To be confirmed** - but based on the [world-famous Euro 2016 sweepstake](https://aornota.github.io/sweepstake.2016/), probably something like:
+ £60 for first place
+ £30 for second place
+ £20 for third place
+ £10 for the [_деревянная ложка_](https://translate.google.co.uk/#auto/en/%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D1%8F%D0%BD%D0%BD%D0%B0%D1%8F%20%D0%BB%D0%BE%D0%B6%D0%BA%D0%B0)"""

let [<Literal>] MARKDOWN_SYNTAX_MARKDOWN = """# Markdown syntax
### A very quick introduction
Text can be:
+ **emboldened**
+ _italicized_
+ **_emboldened and italicized_**
+ ~~struck-through~~

This is a paragraph.
This is part of the same paragraph.

But this is a new paragraph.

This is a picture by the wonderful Gregory Kondos:

![Text if image not found...](https://tinyurl.com/y76sbjyr "Sacremento River with 32 Palms")

This is a list of Mdou Moctar albums:

| Name |   | Released |
|:-----|:-:|---------:|
| [_Sousoume Tamachek_](https://mdoumoctar.bandcamp.com/album/sousoume-tamachek) | ![](https://tinyurl.com/ybjew7oo) | September 2017 |
| [_Akounak Tedalat Taha Tazoughai_](https://mdoumoctar.bandcamp.com/album/akounak-tedalat-taha-tazoughai-ost) (original soundtrack recording) | ![](https://tinyurl.com/y7hgyc77) | June 2015 |
| [_Anar_](https://mdoumoctar.bandcamp.com/album/anar) | ![](https://tinyurl.com/y7r3fby3) | September 2014 |
| [_Afelan_](https://mdoumoctar.bandcamp.com/album/afelan) | ![](https://tinyurl.com/yam6o2zh) | July 2013 |

And here's a Matt Miles quote [from _Dark Mountain_ issue 11]:
> The immigrants of the Global South, the cultures we've turned our backs on even as we profit from
> their labour, are the indicator species of our own societal collapse. The most sensitive and
> susceptible elements of our own species - the ones from whom everything has already been taken,
> the ones who have no recourse to technological mediation, whose subsistence economies have
> already been wrecked by globalization, whose land succumbs to the rising seas, whose societies
> have been destroyed by imperial land grabs and resource wars - they are here now, knocking on
> our front doors, because they have nowhere else to go. On a planet dominated by the movements of
> human beings, we are our own indicator species.
---
Made possible thanks to [Marked.js](https://marked.js.org/#/README.md) and [Maxime Mangel](https://github.com/MangelMaxime/Fulma/blob/master/docs/src/Libs/Fable.Import.Marked.fs)."""
