# Bo Aanes

**OBS:** Pass på at `stack` er installert!

`stack ghci` for å kjøre interaktivt

`stack run` for å kjøre `main` som kjører et eksempel på planlegging

Funksjonen `plan` i `app/Main.hs` tar i mot en liste med stay-consrtaints i prioritert rekkefølge, i tillegg til en liste med must-constraints.
Den vil så finne best plan gitt prioriteringen av stay-constraints.

Funksjonen `methodsToEnforce` i `app/Main.hs` tar i mot en plan og returnerer en liste med rekkefølgen på metodene som må kjøres.

Definisjonen for monoiden finnes i `app/HotDrink.hs`

Eksempeldata finnes i `app/examples` 

