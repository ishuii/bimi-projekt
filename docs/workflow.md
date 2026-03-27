## Workflow-Schritte

**Wichtige Anmerkungen*
- wir pushen unseren Code nicht auf Main, sondern in den Branch "dev"
  - "dev" ist mittlerweile der default-Branch, also sollte es keine Probleme damit geben
- jede Bearbeitung findet zudem auf einem "frischen" Branch statt, der mit "feature/..." beginnt
- die Befehle in der Bash können in RStudio im Terminal verwendet werden (unten neben Console; falls nicht da, dann im Feld rechts unten Files - More - Open New Terminal here)

Voraussetzungen: das Repo wurde bereits geklont und läuft lokal, z.B. über RStudio oder VS Code

### Pull
Vor dem Beginn der Arbeit sollte der lokale Repo-Clone upgedatet werden.

In der Bash: 
```
git pull origin dev
```

In RStudio über den entsprechenden Button (rechts oben auf Git &rarr; Pull wählen). Wichtig: Hierzu muss der Branch "dev" ausgewählt werden!

### Neuen Branch erschaffen
Es ist äußerst wichtig, NICHT einfach so auf main hochzuladen, da es sonst zu gravierenden merge-Fehlern kommen kann. Stattdessen arbeitet jeder zunächst auf seinem eigenen Branch.

In der Bash: 
```
git checkout -b feature/my-feature
```

In RStudio: im Git Fenster oben rechts auf "New Branch" klicken (daneben sollte "dev" stehen, nicht "main"!). Bei Branch Name wird "feature/my-feature" (mit entsprechendem Namen) geschrieben. Dann auf Create klicken. 

Erklärung: "feature" ist damit ein paralleler Branch zu main. Hier darf man auch was "kaputt machen", ohne dass es den Code der anderen beeinträchtigt (das main Projekt bleibt sicher). Statt "my-feature" sollten eindeutige Namen gewählt werden, z.B. `feature/single-linkage` oder `feature/ui-layout`

Jeder Branch folgt dem Zyklus: erschaffen &rarr; arbeiten &rarr; push &rarr; Pull request &rarr; merge &rarr; extra Branch wird gelöscht

### Arbeiten und commit
Nachdem im neuen Branch gearbeitet wurde, müssen die neuen Dateien sowie Veränderungen in bereits bestehenden Dateien commited werden. 

In der Bash:
```
git add .
git commit -m "Commit message"
```
 
In RStudio: Im Fenster Git zunächst bei den bearbeiteten Dateien "Staged" ankreuzen. Dann auf Commit drücken. Es öffnet sich ein Fenster, dass die Veränderungen anzeigt und das Eingeben einer Commit message ermöglicht. Nach Ausfüllen wieder Commit drücken.

*Anmerkung*: Die Commit-Message sollte kurz und möglich eindeutig sein, z.B. "Single Linkage hinzugefügt" oder "UI Layout Update".

### Push
Nun wird die Veränderungen in das allgemeine Repo geschoben.

In der Bash:
```
git push origin feature/my-feature
```
In R-Studio: über den Button "Push"

### Pull Request erschaffen

Dieser Schritt erfolgt auf GitHub. Hier sollte ein Banner angezeigt werden: "Compare & Pull Request" &rarr; darauf klicken.
Falls es nicht angezeigt wird, suchen im Pull Request Tab &rarr; dann auf "New Pull Request"

Dort einstellen:

```
base: dev
compare: feature/my-feature
```

Es sollte ein Titel eingegeben werden (kurz und prägnant, wie die Commit message) und eine Beschreibung dessen, was gemacht worden ist (auch hier so lang wie nötig, so kurz wie möglich halten).

Dann klicken auf "Create Pull Request".

### Review, Merge und Löschen des Branches

Der Code kann dann gereviewt werden &rarr; "Merge Pull Request" klicken und bestätigen.

Im Anschluss muss der feature/my-feature Branch wieder gelöscht werden. GitHub wird diese Option anzeigen, man muss sie nur bestätigen.

### Zurück zu RStudio

Um final alles zu syncen, sollte nun in den Branch "dev" gewechselt werden und dann erneut ein Pull durchgeführt werden. 


