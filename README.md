# AHV-AVS-budget
The scripts and data reproduce the "AHV Finanzhaushalt" expenditure projections published on 16.09.2024 (see https://www.bsv.admin.ch/bsv/de/home/sozialversicherungen/ahv/finanzen-ahv.html).

## Motivation
- DE | Wichtige, komprimierte Bundesdaten im Internet lesbar machen.
- FR | Rendre lisibles sur le Web des données fédérales importantes qui étaient compressées.
- IT | Rendere accessibili sul web importanti dati federali che erano stati compressi.
- EN | Making important federal data that was compressed, readable on the web.

## Context
### Français
Ce repositoire **non-officiel** contient **le code de calcul du budget de l'Assurance-vieillesse et survivants (AVS)** suisse, publié le 31 octobre 2024 par l'Office fédéral des Assurances Sociales (OFAS) ([lien original](https://www.bsv.admin.ch/dam/bsv/fr/dokumente/ahv/finanzperspektiven/berechnungscode-fh-ahv.zip.download.zip/fhh_ahv_160924.zip)).

La publication de ce code fait suite à **une erreur d'estimation de plusieurs milliards des dépenses de l'AVS** causée par le code précédent, menant à une controverse publique durant l'été 2024.

Les fichiers publiés et reproduits ici contiennent **la version corrigée du code**, comprenant les scripts de calcul (en langage de programmation "R"), et les données utilisées pour effectuer ces calculs.

#### Pour plus d'informations
- le [rapport d'enquête](https://www.newsd.admin.ch/newsd/message/attachments/91930.pdf) (en allemand) mené par le cabinet d'avocat Bratschi, dont un [extrait traduit en français](https://www.newsd.admin.ch/newsd/message/attachments/91939.pdf) est disponible, ainsi que le [communiqué de presse](https://www.news.admin.ch/fr/nsb?id=103468) du Département fédéral de l’intérieur (DFI, qui comprend l'OFAS) l'accompagnant
- la [page dédiée de l'OFAS](https://www.bsv.admin.ch/bsv/fr/home/assurances-sociales/ahv/finanzen-ahv.html) sur les calculs financiers de l'AVS

### Deutsch
Dieses **inoffizielle** Repository enthält **den Berechnungscode für das Budget der schweizerischen Alters- und Hinterlassenenversicherung (AHV)**, der am 31. Oktober 2024 vom Bundesamt für Sozialversicherungen (BSV) veröffentlicht wurde ([Original-Link](https://www.bsv.admin.ch/dam/bsv/fr/dokumente/ahv/finanzperspektiven/berechnungscode-fh-ahv.zip.download.zip/fhh_ahv_160924.zip)).

Die Veröffentlichung dieses Codes erfolgte nach **einem Schätzfehler bei den AHV-Ausgaben in Milliardenhöhe**, der durch den vorherigen Code verursacht wurde und im Sommer 2024 zu einer öffentlichen Kontroverse führte.

Die hier veröffentlichten und reproduzierten Dateien enthalten **die korrigierte Version des Codes**, einschliesslich der Berechnungsskripte (in der Programmiersprache «R») und der Daten, die für diese Berechnungen verwendet wurden.

#### Für weitere Informationen
- den [Untersuchungsbericht](https://www.newsd.admin.ch/newsd/message/attachments/91930.pdf), der von der Anwaltskanzlei Bratschi erstellt wurde, sowie die begleitende [Medienmitteilung](https://www.news.admin.ch/de/nsb?id=103468) des Eidgenössischen Departements des Innern (EDI, zu dem auch das BSV gehört)
- die [Themenseite des BSV](https://www.bsv.admin.ch/bsv/fr/home/assurances-sociales/ahv/finanzen-ahv.html) zu den finanziellen Berechnungen der AHV

### Italiano
Questo repository **non ufficiale** contiene **il codice di calcolo del budget dell’Assicurazione vecchiaia e superstiti (AVS)** svizzera, pubblicato il 31 ottobre 2024 dall’Ufficio federale delle assicurazioni sociali (UFAS) ([link originale](https://www.bsv.admin.ch/dam/bsv/fr/dokumente/ahv/finanzperspektiven/berechnungscode-fh-ahv.zip.download.zip/fhh_ahv_160924.zip)).

La pubblicazione di questo codice fa seguito a **un errore di stima delle spese AVS di diversi miliardi**, causato dal codice precedente, che ha portato a una controversia pubblica durante l’estate 2024.

I file pubblicati e riprodotti qui contengono **la versione corretta del codice**, comprendente gli script di calcolo (nel linguaggio di programmazione «R») e i dati utilizzati per effettuare tali calcoli.

#### Per maggiori informazioni
- il [rapporto d’indagine](https://www.newsd.admin.ch/newsd/message/attachments/91930.pdf) (in tedesco), nonché il [comunicato stampa](https://www.news.admin.ch/it/nsb?id=103468) del Dipartimento federale dell’interno (DFI, che comprende anche l’UFAS) che lo accompagna
- la [pagina dedicata dell’UFAS](https://www.bsv.admin.ch/bsv/fr/home/assurances-sociales/ahv/finanzen-ahv.html) sui calcoli finanziari dell’AVS
