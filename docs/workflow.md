## Workflow-Schritte

Voraussetzungen: das Repo wurde bereits geklont und läuft lokal, z.B. über RStudio oder VS Code

### Pull
Vor dem Beginn der Arbeit sollte der lokale Repo-Clone upgedatet werden.

In der Bash: 
```
git pull origin dev
```

In RStudio über den entsprechenden Button (rechts oben auf Git &rarr; Pull wählen)

### Neuen Branch erschaffen
Es ist äußerst wichtig, NICHT einfach so auf main hochzuladen, da es sonst zu gravierenden merge-Fehlern kommen kann. Stattdessen arbeitet jeder zunächst auf seinem eigenen Branch.

In der Bash: 
```
git checkout -b feature/my-feature
```

In RStudio: \ 
\
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

### Pull Request erschaffen


