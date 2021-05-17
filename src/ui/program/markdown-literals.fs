module Aornota.Sweepstake2021.Ui.Program.Markdown.Literals

// TODO-NMB: Review...

let [<Literal>] SCORING_SYSTEM_MARKDOWN = """##### **Please note:**

The scoring system is slightly different from [that used](http://aornota.github.io/sweepstake.2015/scoring.html) for the world-famous Rugby World Cup 2015 sweepstake:
+ **team/coach** points - for **winning** or **drawing** matches; and per **bonus point** earned - are a little higher
+ **player** points - for being named **man-of-the-match**; and for scoring a **try** - are now higher for **forwards** than for **backs**

(There are no changes to points relating to penalty kicks, conversions or drop goals as I can only recall two instances of forwards attempting these: a
[piss-poor effort](https://www.youtube.com/watch?v=XH-aV_36WVM) from Sergio Parisse; and a [comedy conversion](https://www.youtube.com/watch?v=x5b0yMxQ088) from Johnny Redelinghuys.)

These changes are intended to address the [dominance of backs](http://aornota.github.io/sweepstake.2015/index.html#Top%20teams/coaches) in the "top scorer" charts last time.

##### **Scoring system:**

Each sweepstake team will consist of a **team/coach**, **8 forwards** and **7 backs**.

The **team/coach** will score (or lose) points for:
+ **winning** a match: _**12**_ or _**10**_ or _**8**_ (see below)
+ **drawing** a match: _**6**_ or _**5**_ or _**4**_ (see below)
+ per **bonus point** earned: _**4**_ or _**3**_ or _**2**_ (see below)
+ the team scoring a **penalty try**: _**6**_
+ a team player receiving a **yellow card**: _**-2**_
+ a team player receiving a **red card**: _**-4**_

(If a player receives a second yellow card in a match, the two yellow cards will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight"
red card, both cards will be scored.)

Where multiple possible scores are given above, the score will depend on whether the team and their opponents are in the top 8 seeds:
+ if the team **is** a top 8 seed but their opponents are **not**, the **lowest** score will apply
+ if the team is **not** a top 8 seed but their opponents **are**, the **highest** score will apply
+ if **both** teams are top 8 seeds - or if **neither** team is - the **middle** score will apply

The top 8 seeds are (in order): New Zealand; England; Australia; Ireland; Scotland; France; South Africa; and Wales.

The remaining teams are: Argentina; Canada; Fiji; Georgia; Italy; Japan; Namibia; Russia; Samoa; Tonga; United States; and Uruguay.

The team can earn _up to two_ bonus points (weighted as outlined above) for:
+ scoring 4 or more tries in a match
+ losing by 7 points or fewer

The **forwards** and **backs** will score (or lose) points for:
+ being named **man-of-the-match**: _**13**_ for **forwards** and _**10**_ for **backs**
+ scoring a **try**: _**12**_ for **forwards** and _**9**_ for **backs**
+ kicking a **drop goal** or **penalty**: _**3**_
+ kicking a **conversion**: _**2**_
+ _missing_ a **conversion**: _**-1**_
+ _missing_ a **penalty**: _**-2**_
+ receiving a **yellow card**: _**-3**_
+ receiving a **red card**: _**-6**_

(If penalties or conversions are retaken for any reason, only the outcome of the final attempt will be scored. And again, if a player receives a second yellow card in a match, the two
yellow cards will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight" red card, both cards will be scored.)"""

let [<Literal>] DRAFT_ALGORITHM_MARKDOWN = """This is not the easiest thing to explain - so let's try a simplified example:

**neph**, **rosie** and **hugh** submit the following selections for the first draft:

+ **neph**: _1._ Dante; _2._ Goethe; _3._ Saki
+ **rosie**: _1._ Cervantes; _2._ St. Augustine; _3._ Milton
+ **hugh**: _1._ Cervantes; _2._ Dante; _3._ St. Augustine

For the first round, we look at the top selection for each participant. Only **neph** has put Dante first and he gets an uncontested pick. However, both **rosie** and **hugh** fancy Cervantes, so we toss a (metaphorical) coin to resolve this contested pick: **rosie** wins on this occasion; **hugh** has his "pick priority" increased by way of compensation.

So after the first round: **neph** has picked Dante; **rosie** has picked Cervantes; **hugh** remains empty-handed.

Before the second round, we update each participant's list to remove teams / players that have now been picked. (As we're about to find out, this can have some slightly counter-intuitive consequences.)

The updated (and renumbered) selections are:

+ **neph**: _1._ Goethe; _2._ Saki
+ **rosie**: _1._ St. Augustine; _2._ Milton
+ **hugh**: _1._ St. Augustine

**neph** again has a unique selection for the second round and gets Goethe; **rosie** and **hugh** both want St. Augustine - and as **hugh** has the higher "pick priority" (having already lost one coin-toss), he wins automatically.

(Note that **hugh** ends up with St. Augustine even though this was third on his original list yet second on **rosie**'s. What can I say? Shit happens.)

After the second round: **neph** has picked Dante and Goethe; **rosie** has picked Cervantes; **hugh** has picked St. Augustine.

And for the third round, **neph** and **rosie** have uncontested picks (Saki and Milton respectively) and **hugh** has no selections left - so we end up with:

+ **neph** gets Dante, Goethe and Saki
+ **rosie** gets Cervantes and Milton
+ **hugh** gets St. Augustine

(Unfortunately for **neph**, Goethe gives Dante a "wet willy" in a group stage match and is sent home in disgrace; Dante is subsequently sidelined with a nasty ear infection; and Saki claims to have no interest whatsoever in playing football. **rosie**'s hopes are shattered when Cervantes misses a crucial penalty after a Zaza-esque run-up and Milton scores a hat-trick of own goals in the opening match. In the end, **hugh** emerges triumphant as St. Augustine wins the Golden Boot and cryptically claims: _And when by chance prosperity smiled in my direction, I lacked the spirit to seize it, for it fled away almost before I could get my hand upon it_. What a tosser.)

---
It's not a perfect algorithm by any means. But it's the best I've been able to come up with...

For a more detailed example, here are the [first and second draft details](http://aornota.github.io/sweepstake.2015/draft.html) for the world-famous Rugby World Cup 2015 sweepstake."""

let [<Literal>] PAYOUTS_MARKDOWN = """##### **Payouts:**
+ **£TBC** for first place
+ **$TBC** for second place
+ **£TBC** for third place
+ **£TBC** for the wooden spoon"""

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

| Name | Released |   |
|:-----|---------:|:-:|
| [_Ilana: The Creator_](https://mdoumoctar.bandcamp.com/album/ilana-the-creator) | March 2019 | ![](https://tinyurl.com/y3285qgd "Like ZZ Top freaking out with Eddie Van Halen in 1975") |
| [_Blue Stage Session_](https://mdoumoctar.bandcamp.com/album/mdou-moctar-blue-stage-session) | January 2019 | ![](https://tinyurl.com/y6roz6yn "Live in Detroit") |
| [_Sousoume Tamachek_](https://mdoumoctar.bandcamp.com/album/sousoume-tamachek) | September 2017 | ![](https://tinyurl.com/ybjew7oo "Quite possibly my favourite album") |
| [_Akounak Tedalat Taha Tazoughai_](https://mdoumoctar.bandcamp.com/album/akounak-tedalat-taha-tazoughai-ost) (original soundtrack recording) | June 2015 | ![](https://tinyurl.com/y7hgyc77 "Soundtrack to a Tuareg language reimagining of 'Purple Rain'") |
| [_Anar_](https://mdoumoctar.bandcamp.com/album/anar) | September 2014 | ![](https://tinyurl.com/y7r3fby3) |
| [_Afelan_](https://mdoumoctar.bandcamp.com/album/afelan) | July 2013 | ![](https://tinyurl.com/yam6o2zh) |

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
