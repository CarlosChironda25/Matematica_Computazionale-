(* ===================================================== *)
(*                DATABASE DEGLI ESERCIZI                *)

(* Tipo: testo (risposta stringa)                        *)
(*"livello difficoltà ","tipo risposta: testo ","domanda","risposta corretta", {""}*)
(*

"level" ->  livello difficoltà 
"type" ->  tipo risposta: testo 
"question" ->  domanda 
"answer" -> risposta corretta 
"hints" -> { aiuti progressivi per l’utente (primo, secondo, terzo)
*)
(* ===================================================== *)

{
 <|"level"->1,"type"->"text","question"->"Come si chiama la stella al centro del sistema solare?","answer"->"sole","hints"->{"Illumina tutti i pianeti","È la stella centrale del sistema solare","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta viviamo?","answer"->"terra","hints"->{"È il nostro pianeta","Ha acqua e vita","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è rosso?","answer"->"marte","hints"->{"È chiamato pianeta rosso","È il quarto pianeta dal Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha gli anelli?","answer"->"saturno","hints"->{"Ha cerchi intorno","È un gigante gassoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più grande?","answer"->"giove","hints"->{"È il più grande del sistema solare","È un gigante gassoso","Inizia con G"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è molto vicino al Sole?","answer"->"mercurio","hints"->{"È il primo pianeta","È il più vicino al Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è molto caldo?","answer"->"venere","hints"->{"È il secondo pianeta","Ha effetto serra molto forte","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è un pianeta o una stella?","answer"->"pianeta","hints"->{"Gira intorno al Sole","Non produce luce","È un corpo celeste"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è un pianeta o una stella?","answer"->"stella","hints"->{"Produce luce e energia","È al centro del sistema solare","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Come si chiama il satellite della Terra?","answer"->"luna","hints"->{"Si vede di notte","Gira intorno alla Terra","Inizia con L"}|>,

<|"level"->1,"type"->"text","question"->"Quanti pianeti ci sono nel sistema solare?","answer"->"8","hints"->{"Sono meno di 10","Più di 7","Numero ufficiale"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è blu?","answer"->"terra","hints"->{"Ha molta acqua","È il nostro pianeta","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Marte è vicino o lontano dal Sole?","answer"->"lontano","hints"->{"È il quarto pianeta","Non è tra i primi","è tra gli ultimi"}|>,
<|"level"->1,"type"->"text","question"->"Mercurio è vicino o lontano dal Sole?","answer"->"vicino","hints"->{"È tra i primi pianeti","È molto caldo","È il primo pianeta"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è caldo o freddo?","answer"->"caldo","hints"->{"Produce energia","Temperatura altissima","Non è freddo"}|>,

<|"level"->1,"type"->"text","question"->"La Luna è grande o piccola rispetto alla Terra?","answer"->"piccola","hints"->{"È più piccola della Terra","È un satellite naturale","La Terra è più grande"}|>,
<|"level"->1,"type"->"text","question"->"Giove è grande o piccolo?","answer"->"grande","hints"->{"È il più grande pianeta","Gigante gassoso","È il più grande pianeta"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il secondo dal Sole?","answer"->"venere","hints"->{"Dopo Mercurio","È molto caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il terzo dal Sole?","answer"->"terra","hints"->{"Il nostro pianeta","Ha vita","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il quarto dal Sole?","answer"->"marte","hints"->{"Pianeta rosso","Dopo la Terra","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il quinto dal Sole?","answer"->"giove","hints"->{"Gigante gassoso","Molto grande","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il sesto dal Sole?","answer"->"saturno","hints"->{"Ha anelli","Gigante","Inizia con S"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il settimo dal Sole?","answer"->"urano","hints"->{"Molto freddo","Gira inclinato","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è l'ottavo dal Sole?","answer"->"nettuno","hints"->{"Il più lontano","Molto freddo","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a lunedì?","answer"->"luna","hints"->{"Nome del giorno","Satellite della Terra","Inizia con L"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a martedì?","answer"->"marte","hints"->{"Nome simile","Pianeta rosso","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a mercoledì?","answer"->"mercurio","hints"->{"Inizia con merc","Primo pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a giovedì?","answer"->"giove","hints"->{"Nome simile","Gigante","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a venerdì?","answer"->"venere","hints"->{"Inizia con vene","Pianeta caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha acqua. sì o no?","answer"->"sì","hints"->{"Mari e oceani","Copre gran parte del pianeta","credo"}|>,
<|"level"->1,"type"->"text","question"->"Marte è rosso. sì o no?","answer"->"sì","hints"->{"Pianeta rosso","Colore caratteristico","Credo"}|>,
<|"level"->1,"type"->"text","question"->"Venere è caldo o freddo?","answer"->"caldo","hints"->{"Effetto serra","Temperatura altissima","Sei vicino"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il più lontano dal Sole?","answer"->"nettuno","hints"->{"È l'ottavo pianeta","Il più distante","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il più vicino alla Terra?","answer"->"venere","hints"->{"Molto vicino alla Terra","Secondo pianeta","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il primo pianeta dal Sole?","answer"->"mercurio","hints"->{"Il più vicino al Sole","Primo pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta dopo Venere?","answer"->"terra","hints"->{"Terzo pianeta","Il nostro pianeta","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta prima di Giove?","answer"->"marte","hints"->{"Pianeta rosso","Quarto pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta dopo Giove?","answer"->"saturno","hints"->{"Ha anelli","Gigante gassoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta prima di Nettuno?","answer"->"urano","hints"->{"Molto freddo","Settimo pianeta","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha aria. sì o no?","answer"->"sì","hints"->{"Atmosfera terrestre","Permette la vita","credo"}|>,
<|"level"->1,"type"->"text","question"->"Marte ha acqua liquida. sì o no?","answer"->"no","hints"->{"Solo tracce di ghiaccio","Non stabile","non credo"}|>,

<|"level"->1,"type"->"text","question"->"Qual è il pianeta con anelli grandi?","answer"->"saturno","hints"->{"Anelli visibili","Molto famoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più freddo?","answer"->"urano","hints"->{"Molto lontano dal Sole","Basse temperature","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più caldo?","answer"->"venere","hints"->{"Effetto serra forte","Altissima temperatura","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha tempeste grandi?","answer"->"giove","hints"->{"Grande macchia rossa","Gigante gassoso","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha vento forte?","answer"->"nettuno","hints"->{"Venti estremi","Molto lontano dal Sole","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è inclinato molto?","answer"->"urano","hints"->{"Asse quasi orizzontale","Rotazione particolare","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha atmosfera densa?","answer"->"venere","hints"->{"Effetto serra forte","Atmosfera molto spessa","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha molti crateri?","answer"->"mercurio","hints"->{"Superficie antica","Molti impatti","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è dopo Marte?","answer"->"giove","hints"->{"Gigante del sistema solare","Molto grande","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima di Saturno?","answer"->"giove","hints"->{"Pianeta gigante","Prima di Saturno","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è dopo Urano?","answer"->"nettuno","hints"->{"Ultimo pianeta","Molto freddo","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima di Venere?","answer"->"mercurio","hints"->{"Primo pianeta","Molto vicino al Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima della Terra?","answer"->"venere","hints"->{"Secondo pianeta","Molto caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Giove è grande o piccolo?","answer"->"grande","hints"->{"È il più grande pianeta","Gigante gassoso","È il re dei pianeti per dimensioni"}|>,
<|"level"->1,"type"->"text","question"->"Saturno ha anelli. sì o no?","answer"->"sì","hints"->{"Ha anelli visibili","È un gigante gassoso","Si vede un disco intorno al pianeta"}|>,
<|"level"->1,"type"->"text","question"->"La Terra gira intorno al Sole. Sì o no?","answer"->"sì","hints"->{"Sistema eliocentrico","Orbita il Sole","È il moto di rivoluzione"}|>,
<|"level"->1,"type"->"text","question"->"Il sistema solare è nello spazio o nel mare?","answer"->"spazio","hints"->{"Fuori dalla Terra","Non è acqua","Si trova tra le stelle"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è al centro del sistema solare. sì o no?","answer"->"sì","hints"->{"Tutti girano intorno","Centro del sistema","È la stella principale"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha acqua. sì o no?","answer"->"sì","hints"->{"Mari e oceani","Copre gran parte del pianeta","È chiamata il pianeta blu"}|>,

<|"level"->1,"type"->"text","question"->"Venere è caldo o freddo?","answer"->"caldo","hints"->{"Effetto serra","Temperatura altissima","È il pianeta più rovente"}|>,
<|"level"->1,"type"->"text","question"->"Nettuno è caldo o freddo?","answer"->"freddo","hints"->{"Molto lontano","Temperature basse","Si trova ai confini del sistema"}|>,
<|"level"->1,"type"->"text","question"->"Urano è vicino o lontano dal Sole?","answer"->"lontano","hints"->{"Quasi ultimo pianeta","Molto distante","Oltre Saturno"}|>,

<|"level"->1,"type"->"text","question"->"Giove ha anelli. sì o no?","answer"->"no","hints"->{"Non visibili come Saturno","Non ha anelli evidenti","Sono quasi invisibili"}|>,
<|"level"->1,"type"->"text","question"->"Saturno è grande o piccolo?","answer"->"grande","hints"->{"Uno dei più grandi pianeti","Gigante gassoso","Viene subito dopo Giove"}|>,
<|"level"->1,"type"->"text","question"->"Mercurio è caldo o freddo?","answer"->"caldo","hints"->{"Molto vicino al Sole","Temperatura alta","È il primo pianeta"}|>,
<|"level"->1,"type"->"text","question"->"La Luna gira intorno alla Terra. sì o no?","answer"->"sì","hints"->{"È il satellite naturale","Orbita la Terra","La vediamo cambiare fase ogni mese"}|>,
<|"level"->1,"type"->"text","question"->"La Terra gira su se stessa. sì o no?","answer"->"sì","hints"->{"Causa giorno e notte","Rotazione terrestre","Dura 24 ore"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole si muove intorno alla Terra. sì o no?","answer"->"no","hints"->{"È il contrario","Sistema eliocentrico","La Terra è quella che orbita"}|>,
<|"level"->1,"type"->"text","question"->"Il cielo notturno è pieno di stelle. sì o no?","answer"->"sì","hints"->{"Molte stelle visibili","Spazio profondo","Si vedono quando non c'è luce"}|>,
<|"level"->1,"type"->"text","question"->"La Luna brilla di luce propria. sì o no?","answer"->"no","hints"->{"Riflette la luce del Sole","Non produce luce","È come uno specchio nel cielo"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il più vicino alla Terra?","answer"->"venere","hints"->{"Molto vicino alla Terra","Secondo pianeta","Spesso chiamato gemello della Terra"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è blu per cosa?","answer"->"acqua","hints"->{"Copre la maggior parte del pianeta","Mari e oceani","Senza di lei non ci sarebbe vita"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole dà luce. sì o no?","answer"->"sì","hints"->{"Illumina tutto il sistema solare","Fonte di energia","Senza di lui sarebbe sempre buio"}|>,

<|"level"->1,"type"->"text","question"->"Mercurio è piccolo o grande?","answer"->"piccolo","hints"->{"È il più piccolo pianeta","Dimensione ridotta","È poco più grande della Luna"}|>,
<|"level"->1,"type"->"text","question"->"Giove è piccolo o grande?","answer"->"grande","hints"->{"Il più grande del sistema solare","Gigante","Potrebbe contenere tutti gli altri pianeti"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è calda o fredda?","answer"->"temperata","hints"->{"Non estrema","Condizioni equilibrate","Permette la vita"}|>,

<|"level"->1,"type"->"text","question"->"Nettuno è blu. sì o no?","answer"->"sì","hints"->{"Colore blu intenso","Pianeta distante","Ha il colore del mare profondo"}|>,
<|"level"->1,"type"->"text","question"->"Urano è blu. sì o no?","answer"->"sì","hints"->{"Simile a Nettuno","Colore azzurro","È un gigante di ghiaccio"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è una stella. sì o no?","answer"->"sì","hints"->{"Produce energia","Centro del sistema solare","È una nana gialla"}|>,
<|"level"->1,"type"->"text","question"->"La Luna è una stella. sì o no?","answer"->"no","hints"->{"È un satellite naturale","Non produce luce","Gira intorno a noi"}|>,

<|"level"->1,"type"->"text","question"->"Marte ha acqua liquida. sì o no?","answer"->"no","hints"->{"Solo tracce di ghiaccio","Non stabile","In passato forse l'aveva"}|>,
<|"level"->1,"type"->"text","question"->"Venere ha nuvole. sì o no?","answer"->"sì","hints"->{"Atmosfera densa","Molte nuvole","Riflettono molta luce"}|>,

<|"level"->1,"type"->"text","question"->"La Terra ha una luna o più lune?","answer"->"una","hints"->{"Un solo satellite","Gira intorno alla Terra","Si chiama semplicemente Luna"}|>,
<|"level"->1,"type"->"text","question"->"Giove ha molte lune. sì o no?","answer"->"sì","hints"->{"Molte lune","Gigante gassoso","Ne ha quasi 100"}|>,
<|"level"->1,"type"->"text","question"->"Saturno ha lune. sì o no?","answer"->"sì","hints"->{"Molte lune","Sistema complesso","Titano è la più famosa"}|>,

<|"level"->1,"type"->"text","question"->"Mercurio ha lune. sì o no?","answer"->"no","hints"->{"Zero satelliti","Molto piccolo","È troppo vicino al Sole"}|>,
<|"level"->1,"type"->"text","question"->"Venere ha lune. sì o no?","answer"->"no","hints"->{"Nessuna luna","Atmosfera densa","Non ha satelliti naturali"}|>,

<|"level"->1,"type"->"text","question"->"Il giorno è quando c'è Sole. sì o no?","answer"->"sì","hints"->{"Luce solare","Parte del ciclo giorno-notte","Quando ci svegliamo"}|>,
<|"level"->1,"type"->"text","question"->"La notte è senza Sole. sì o no?","answer"->"sì","hints"->{"Buio","Il Sole non è visibile","Quando andiamo a dormire"}|>,

<|"level"->1,"type"->"text","question"->"Il sistema solare ha 8 pianeti. sì o no?","answer"->"sì","hints"->{"Numero attualmente accettato","Dopo Plutone escluso","Contando da Mercurio a Nettuno"}|>,
<|"level"->1,"type"->"text","question"->"Plutone è un pianeta principale. sì o no?","answer"->"no","hints"->{"È un pianeta nano","Non più classificato principale dal 2006","È troppo piccolo"}|>,
<|"level"->1,"type"->"text","question"->"Le stelle sono lontane. sì o no?","answer"->"sì","hints"->{"Distanze enormi nello spazio","Non nel sistema solare","Le vediamo come piccoli puntini"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è vicino alla Terra. sì o no?","answer"->"sì","hints"->{"È la nostra stella","Distanza astronomica relativamente piccola","La sua luce ci mette 8 minuti ad arrivare"}|>,
<|"level"->1,"type"->"text","question"->"Lo spazio è vuoto. sì o no?","answer"->"sì","hints"->{"Quasi nessuna materia","Vuoto parziale","Non c'è aria da respirare"}|>,

<|"level"->1,"type"->"text","question"->"La Terra è terza dal Sole. sì o no?","answer"->"sì","hints"->{"Ordine corretto dei pianeti","Dopo Venere","Viene prima di Marte"}|>,
<|"level"->1,"type"->"text","question"->"Marte è quarto dal Sole. sì o no?","answer"->"sì","hints"->{"È il quarto pianeta","Pianeta rosso","Viene dopo la Terra"}|>,
<|"level"->1,"type"->"text","question"->"Giove è quinto dal Sole. sì o no?","answer"->"sì","hints"->{"Gigante gassoso","Quinta posizione","Dopo la fascia degli asteroidi"}|>,
<|"level"->1,"type"->"text","question"->"Saturno è sesto dal Sole. sì o no?","answer"->"sì","hints"->{"Ha anelli","Sesta posizione","Tra Giove e Urano"}|>,
<|"level"->1,"type"->"text","question"->"Nettuno è il primo pianeta. sì o no?","answer"->"no","hints"->{"È l’ottavo pianeta","Più lontano dal Sole","Il primo è Mercurio"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è giallo. sì o no?","answer"->"sì","hints"->{"Colore percepito dalla Terra","Stella di tipo G","È il colore classico nei disegni"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è verde. sì o no?","answer"->"no","hints"->{"È principalmente blu","Presenza di acqua","Ha solo parti verdi (foreste)"}|>,
<|"level"->1,"type"->"text","question"->"Marte è blu. sì o no?","answer"->"no","hints"->{"Pianeta rosso","Ossido di ferro","Sembra ruggine"}|>,
<|"level"->1,"type"->"text","question"->"Venere è brillante. sì o no?","answer"->"sì","hints"->{"Molto luminoso nel cielo","Riflette la luce del Sole","Chiamato Stella del Mattino"}|>,
<|"level"->1,"type"->"text","question"->"La Luna è bianca. sì o no?","answer"->"sì","hints"->{"Riflette la luce solare","Colore chiaro visibile","Sembra un sasso grigio chiaro"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è nel centro. sì o no?","answer"->"sì","hints"->{"Sistema eliocentrico","Centro del sistema solare","Tutto gli gira intorno"}|>,
<|"level"->1,"type"->"text","question"->"I pianeti fanno luce. sì o no?","answer"->"no","hints"->{"Riflettono la luce","Non sono stelle","Non bruciano come il Sole"}|>,
<|"level"->1,"type"->"text","question"->"Le stelle fanno luce. sì o no?","answer"->"sì","hints"->{"Producono energia","Reazioni nucleari","Brillano di luce propria"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha vita. sì o no?","answer"->"sì","hints"->{"Presenza di acqua e atmosfera","Condizioni favorevoli","Ci siamo noi"}|>,
<|"level"->1,"type"->"text","question"->"Marte ha vita certa. sì o no?","answer"->"no","hints"->{"Non confermata","Solo ipotesi","Stiamo ancora mandando i Rover a cercare"}|>,
<|"level"->1,"type"->"text","question"->"Quanti pianeti ci sono nel sistema solare?","answer"->"otto","hints"->{"Conta da Mercurio a Nettuno","Non includere Plutone","È il numero dopo il sette"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il primo pianeta dal Sole?","answer"->"mercurio","hints"->{"Il più vicino alla stella","Piccolo e roccioso","Inizia con la lettera M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il secondo pianeta dal Sole?","answer"->"venere","hints"->{"Pianeta molto caldo","Brilla molto nel cielo","Inizia con la lettera V"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il terzo pianeta dal Sole?","answer"->"terra","hints"->{"Il pianeta dove abitiamo","L'unico con molta acqua liquida","Inizia con la lettera T"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il quarto pianeta dal Sole?","answer"->"marte","hints"->{"Il pianeta rosso","Ha due piccole lune","Inizia con la lettera M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il quinto pianeta dal Sole?","answer"->"giove","hints"->{"Il primo dei giganti gassosi","Il più grande di tutti","Inizia con la lettera G"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il sesto pianeta dal Sole?","answer"->"saturno","hints"->{"Famoso per i suoi grandi anelli","Un gigante gassoso","Inizia con la lettera S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il settimo pianeta dal Sole?","answer"->"urano","hints"->{"Un gigante di ghiaccio azzurro","Ruota su un fianco","Inizia con la lettera U"}|>,
<|"level"->1,"type"->"text","question"->"Qual è l'ottavo pianeta dal Sole?","answer"->"nettuno","hints"->{"Il più lontano dal centro","Molto freddo e ventoso","Inizia con la lettera N"}|>,

  (* Fine database 1A*)
   <|"level"->1,"type"->"text","question"->"Come si chiama la stella al centro del sistema solare?","answer"->"sole","hints"->{"Illumina tutti i pianeti","È la stella centrale del sistema solare","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta viviamo?","answer"->"terra","hints"->{"È il nostro pianeta","Ha acqua e vita","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è rosso?","answer"->"marte","hints"->{"È chiamato pianeta rosso","È il quarto pianeta dal Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha gli anelli?","answer"->"saturno","hints"->{"Ha cerchi intorno","È un gigante gassoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più grande?","answer"->"giove","hints"->{"È il più grande del sistema solare","È un gigante gassoso","Inizia con G"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è molto vicino al Sole?","answer"->"mercurio","hints"->{"È il primo pianeta","È il più vicino al Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è molto caldo?","answer"->"venere","hints"->{"È il secondo pianeta","Ha effetto serra molto forte","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è un pianeta o una stella?","answer"->"pianeta","hints"->{"Gira intorno al Sole","Non produce luce","È un corpo celeste"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è un pianeta o una stella?","answer"->"stella","hints"->{"Produce luce e energia","È al centro del sistema solare","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Come si chiama il satellite della Terra?","answer"->"luna","hints"->{"Si vede di notte","Gira intorno alla Terra","Inizia con L"}|>,

<|"level"->1,"type"->"text","question"->"Quanti pianeti ci sono nel sistema solare?","answer"->"8","hints"->{"Sono meno di 10","Più di 7","Numero ufficiale"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è blu?","answer"->"terra","hints"->{"Ha molta acqua","È il nostro pianeta","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Marte è vicino o lontano dal Sole?","answer"->"lontano","hints"->{"È il quarto pianeta","Non è tra i primi","è tra gli ultimi"}|>,
<|"level"->1,"type"->"text","question"->"Mercurio è vicino o lontano dal Sole?","answer"->"vicino","hints"->{"È tra i primi pianeti","È molto caldo","È il primo pianeta"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è caldo o freddo?","answer"->"caldo","hints"->{"Produce energia","Temperatura altissima","Non è freddo"}|>,

<|"level"->1,"type"->"text","question"->"La Luna è grande o piccola rispetto alla Terra?","answer"->"piccola","hints"->{"È più piccola della Terra","È un satellite naturale","La Terra è più grande"}|>,
<|"level"->1,"type"->"text","question"->"Giove è grande o piccolo?","answer"->"grande","hints"->{"È il più grande pianeta","Gigante gassoso","È il più grande pianeta"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il secondo dal Sole?","answer"->"venere","hints"->{"Dopo Mercurio","È molto caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il terzo dal Sole?","answer"->"terra","hints"->{"Il nostro pianeta","Ha vita","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il quarto dal Sole?","answer"->"marte","hints"->{"Pianeta rosso","Dopo la Terra","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il quinto dal Sole?","answer"->"giove","hints"->{"Gigante gassoso","Molto grande","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il sesto dal Sole?","answer"->"saturno","hints"->{"Ha anelli","Gigante","Inizia con S"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il settimo dal Sole?","answer"->"urano","hints"->{"Molto freddo","Gira inclinato","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è l'ottavo dal Sole?","answer"->"nettuno","hints"->{"Il più lontano","Molto freddo","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a lunedì?","answer"->"luna","hints"->{"Nome del giorno","Satellite della Terra","Inizia con L"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a martedì?","answer"->"marte","hints"->{"Nome simile","Pianeta rosso","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a mercoledì?","answer"->"mercurio","hints"->{"Inizia con merc","Primo pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a giovedì?","answer"->"giove","hints"->{"Nome simile","Gigante","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è associato a venerdì?","answer"->"venere","hints"->{"Inizia con vene","Pianeta caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha acqua. sì o no?","answer"->"sì","hints"->{"Mari e oceani","Copre gran parte del pianeta","credo"}|>,
<|"level"->1,"type"->"text","question"->"Marte è rosso. sì o no?","answer"->"sì","hints"->{"Pianeta rosso","Colore caratteristico","Credo"}|>,
<|"level"->1,"type"->"text","question"->"Venere è caldo o freddo?","answer"->"caldo","hints"->{"Effetto serra","Temperatura altissima","Sei vicino"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il più lontano dal Sole?","answer"->"nettuno","hints"->{"È l'ottavo pianeta","Il più distante","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è il più vicino alla Terra?","answer"->"venere","hints"->{"Molto vicino alla Terra","Secondo pianeta","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il primo pianeta dal Sole?","answer"->"mercurio","hints"->{"Il più vicino al Sole","Primo pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta dopo Venere?","answer"->"terra","hints"->{"Terzo pianeta","Il nostro pianeta","Inizia con T"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta prima di Giove?","answer"->"marte","hints"->{"Pianeta rosso","Quarto pianeta","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta dopo Giove?","answer"->"saturno","hints"->{"Ha anelli","Gigante gassoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta prima di Nettuno?","answer"->"urano","hints"->{"Molto freddo","Settimo pianeta","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha aria. sì o no?","answer"->"sì","hints"->{"Atmosfera terrestre","Permette la vita","credo"}|>,
<|"level"->1,"type"->"text","question"->"Marte ha acqua liquida. sì o no?","answer"->"no","hints"->{"Solo tracce di ghiaccio","Non stabile","non credo"}|>,

<|"level"->1,"type"->"text","question"->"Qual è il pianeta con anelli grandi?","answer"->"saturno","hints"->{"Anelli visibili","Molto famoso","Inizia con S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più freddo?","answer"->"urano","hints"->{"Molto lontano dal Sole","Basse temperature","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il pianeta più caldo?","answer"->"venere","hints"->{"Effetto serra forte","Altissima temperatura","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha tempeste grandi?","answer"->"giove","hints"->{"Grande macchia rossa","Gigante gassoso","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha vento forte?","answer"->"nettuno","hints"->{"Venti estremi","Molto lontano dal Sole","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è inclinato molto?","answer"->"urano","hints"->{"Asse quasi orizzontale","Rotazione particolare","Inizia con U"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha atmosfera densa?","answer"->"venere","hints"->{"Effetto serra forte","Atmosfera molto spessa","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta ha molti crateri?","answer"->"mercurio","hints"->{"Superficie antica","Molti impatti","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è dopo Marte?","answer"->"giove","hints"->{"Gigante del sistema solare","Molto grande","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima di Saturno?","answer"->"giove","hints"->{"Pianeta gigante","Prima di Saturno","Inizia con G"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è dopo Urano?","answer"->"nettuno","hints"->{"Ultimo pianeta","Molto freddo","Inizia con N"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima di Venere?","answer"->"mercurio","hints"->{"Primo pianeta","Molto vicino al Sole","Inizia con M"}|>,
<|"level"->1,"type"->"text","question"->"Quale pianeta è prima della Terra?","answer"->"venere","hints"->{"Secondo pianeta","Molto caldo","Inizia con V"}|>,
<|"level"->1,"type"->"text","question"->"Giove è grande o piccolo?","answer"->"grande","hints"->{"È il più grande pianeta","Gigante gassoso","È il re dei pianeti per dimensioni"}|>,
<|"level"->1,"type"->"text","question"->"Saturno ha anelli. sì o no?","answer"->"sì","hints"->{"Ha anelli visibili","È un gigante gassoso","Si vede un disco intorno al pianeta"}|>,
<|"level"->1,"type"->"text","question"->"La Terra gira intorno al Sole. Sì o no?","answer"->"sì","hints"->{"Sistema eliocentrico","Orbita il Sole","È il moto di rivoluzione"}|>,
<|"level"->1,"type"->"text","question"->"Il sistema solare è nello spazio o nel mare?","answer"->"spazio","hints"->{"Fuori dalla Terra","Non è acqua","Si trova tra le stelle"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è al centro del sistema solare. sì o no?","answer"->"sì","hints"->{"Tutti girano intorno","Centro del sistema","È la stella principale"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha acqua. sì o no?","answer"->"sì","hints"->{"Mari e oceani","Copre gran parte del pianeta","È chiamata il pianeta blu"}|>,

<|"level"->1,"type"->"text","question"->"Venere è caldo o freddo?","answer"->"caldo","hints"->{"Effetto serra","Temperatura altissima","È il pianeta più rovente"}|>,
<|"level"->1,"type"->"text","question"->"Nettuno è caldo o freddo?","answer"->"freddo","hints"->{"Molto lontano","Temperature basse","Si trova ai confini del sistema"}|>,
<|"level"->1,"type"->"text","question"->"Urano è vicino o lontano dal Sole?","answer"->"lontano","hints"->{"Quasi ultimo pianeta","Molto distante","Oltre Saturno"}|>,

<|"level"->1,"type"->"text","question"->"Giove ha anelli. sì o no?","answer"->"no","hints"->{"Non visibili come Saturno","Non ha anelli evidenti","Sono quasi invisibili"}|>,
<|"level"->1,"type"->"text","question"->"Saturno è grande o piccolo?","answer"->"grande","hints"->{"Uno dei più grandi pianeti","Gigante gassoso","Viene subito dopo Giove"}|>,
<|"level"->1,"type"->"text","question"->"Mercurio è caldo o freddo?","answer"->"caldo","hints"->{"Molto vicino al Sole","Temperatura alta","È il primo pianeta"}|>,
<|"level"->1,"type"->"text","question"->"La Luna gira intorno alla Terra. sì o no?","answer"->"sì","hints"->{"È il satellite naturale","Orbita la Terra","La vediamo cambiare fase ogni mese"}|>,
<|"level"->1,"type"->"text","question"->"La Terra gira su se stessa. sì o no?","answer"->"sì","hints"->{"Causa giorno e notte","Rotazione terrestre","Dura 24 ore"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole si muove intorno alla Terra. sì o no?","answer"->"no","hints"->{"È il contrario","Sistema eliocentrico","La Terra è quella che orbita"}|>,
<|"level"->1,"type"->"text","question"->"Il cielo notturno è pieno di stelle. sì o no?","answer"->"sì","hints"->{"Molte stelle visibili","Spazio profondo","Si vedono quando non c'è luce"}|>,
<|"level"->1,"type"->"text","question"->"La Luna brilla di luce propria. sì o no?","answer"->"no","hints"->{"Riflette la luce del Sole","Non produce luce","È come uno specchio nel cielo"}|>,

<|"level"->1,"type"->"text","question"->"Quale pianeta è il più vicino alla Terra?","answer"->"venere","hints"->{"Molto vicino alla Terra","Secondo pianeta","Spesso chiamato gemello della Terra"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è blu per cosa?","answer"->"acqua","hints"->{"Copre la maggior parte del pianeta","Mari e oceani","Senza di lei non ci sarebbe vita"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole dà luce. sì o no?","answer"->"sì","hints"->{"Illumina tutto il sistema solare","Fonte di energia","Senza di lui sarebbe sempre buio"}|>,

<|"level"->1,"type"->"text","question"->"Mercurio è piccolo o grande?","answer"->"piccolo","hints"->{"È il più piccolo pianeta","Dimensione ridotta","È poco più grande della Luna"}|>,
<|"level"->1,"type"->"text","question"->"Giove è piccolo o grande?","answer"->"grande","hints"->{"Il più grande del sistema solare","Gigante","Potrebbe contenere tutti gli altri pianeti"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è calda o fredda?","answer"->"temperata","hints"->{"Non estrema","Condizioni equilibrate","Permette la vita"}|>,

<|"level"->1,"type"->"text","question"->"Nettuno è blu. sì o no?","answer"->"sì","hints"->{"Colore blu intenso","Pianeta distante","Ha il colore del mare profondo"}|>,
<|"level"->1,"type"->"text","question"->"Urano è blu. sì o no?","answer"->"sì","hints"->{"Simile a Nettuno","Colore azzurro","È un gigante di ghiaccio"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è una stella. sì o no?","answer"->"sì","hints"->{"Produce energia","Centro del sistema solare","È una nana gialla"}|>,
<|"level"->1,"type"->"text","question"->"La Luna è una stella. sì o no?","answer"->"no","hints"->{"È un satellite naturale","Non produce luce","Gira intorno a noi"}|>,

<|"level"->1,"type"->"text","question"->"Marte ha acqua liquida. sì o no?","answer"->"no","hints"->{"Solo tracce di ghiaccio","Non stabile","In passato forse l'aveva"}|>,
<|"level"->1,"type"->"text","question"->"Venere ha nuvole. sì o no?","answer"->"sì","hints"->{"Atmosfera densa","Molte nuvole","Riflettono molta luce"}|>,

<|"level"->1,"type"->"text","question"->"La Terra ha una luna o più lune?","answer"->"una","hints"->{"Un solo satellite","Gira intorno alla Terra","Si chiama semplicemente Luna"}|>,
<|"level"->1,"type"->"text","question"->"Giove ha molte lune. sì o no?","answer"->"sì","hints"->{"Molte lune","Gigante gassoso","Ne ha quasi 100"}|>,
<|"level"->1,"type"->"text","question"->"Saturno ha lune. sì o no?","answer"->"sì","hints"->{"Molte lune","Sistema complesso","Titano è la più famosa"}|>,

<|"level"->1,"type"->"text","question"->"Mercurio ha lune. sì o no?","answer"->"no","hints"->{"Zero satelliti","Molto piccolo","È troppo vicino al Sole"}|>,
<|"level"->1,"type"->"text","question"->"Venere ha lune. sì o no?","answer"->"no","hints"->{"Nessuna luna","Atmosfera densa","Non ha satelliti naturali"}|>,

<|"level"->1,"type"->"text","question"->"Il giorno è quando c'è Sole. sì o no?","answer"->"sì","hints"->{"Luce solare","Parte del ciclo giorno-notte","Quando ci svegliamo"}|>,
<|"level"->1,"type"->"text","question"->"La notte è senza Sole. sì o no?","answer"->"sì","hints"->{"Buio","Il Sole non è visibile","Quando andiamo a dormire"}|>,

<|"level"->1,"type"->"text","question"->"Il sistema solare ha 8 pianeti. sì o no?","answer"->"sì","hints"->{"Numero attualmente accettato","Dopo Plutone escluso","Contando da Mercurio a Nettuno"}|>,
<|"level"->1,"type"->"text","question"->"Plutone è un pianeta principale. sì o no?","answer"->"no","hints"->{"È un pianeta nano","Non più classificato principale dal 2006","È troppo piccolo"}|>,
<|"level"->1,"type"->"text","question"->"Le stelle sono lontane. sì o no?","answer"->"sì","hints"->{"Distanze enormi nello spazio","Non nel sistema solare","Le vediamo come piccoli puntini"}|>,
<|"level"->1,"type"->"text","question"->"Il Sole è vicino alla Terra. sì o no?","answer"->"sì","hints"->{"È la nostra stella","Distanza astronomica relativamente piccola","La sua luce ci mette 8 minuti ad arrivare"}|>,
<|"level"->1,"type"->"text","question"->"Lo spazio è vuoto. sì o no?","answer"->"sì","hints"->{"Quasi nessuna materia","Vuoto parziale","Non c'è aria da respirare"}|>,

<|"level"->1,"type"->"text","question"->"La Terra è terza dal Sole. sì o no?","answer"->"sì","hints"->{"Ordine corretto dei pianeti","Dopo Venere","Viene prima di Marte"}|>,
<|"level"->1,"type"->"text","question"->"Marte è quarto dal Sole. sì o no?","answer"->"sì","hints"->{"È il quarto pianeta","Pianeta rosso","Viene dopo la Terra"}|>,
<|"level"->1,"type"->"text","question"->"Giove è quinto dal Sole. sì o no?","answer"->"sì","hints"->{"Gigante gassoso","Quinta posizione","Dopo la fascia degli asteroidi"}|>,
<|"level"->1,"type"->"text","question"->"Saturno è sesto dal Sole. sì o no?","answer"->"sì","hints"->{"Ha anelli","Sesta posizione","Tra Giove e Urano"}|>,
<|"level"->1,"type"->"text","question"->"Nettuno è il primo pianeta. sì o no?","answer"->"no","hints"->{"È l’ottavo pianeta","Più lontano dal Sole","Il primo è Mercurio"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è giallo. sì o no?","answer"->"sì","hints"->{"Colore percepito dalla Terra","Stella di tipo G","È il colore classico nei disegni"}|>,
<|"level"->1,"type"->"text","question"->"La Terra è verde. sì o no?","answer"->"no","hints"->{"È principalmente blu","Presenza di acqua","Ha solo parti verdi (foreste)"}|>,
<|"level"->1,"type"->"text","question"->"Marte è blu. sì o no?","answer"->"no","hints"->{"Pianeta rosso","Ossido di ferro","Sembra ruggine"}|>,
<|"level"->1,"type"->"text","question"->"Venere è brillante. sì o no?","answer"->"sì","hints"->{"Molto luminoso nel cielo","Riflette la luce del Sole","Chiamato Stella del Mattino"}|>,
<|"level"->1,"type"->"text","question"->"La Luna è bianca. sì o no?","answer"->"sì","hints"->{"Riflette la luce solare","Colore chiaro visibile","Sembra un sasso grigio chiaro"}|>,

<|"level"->1,"type"->"text","question"->"Il Sole è nel centro. sì o no?","answer"->"sì","hints"->{"Sistema eliocentrico","Centro del sistema solare","Tutto gli gira intorno"}|>,
<|"level"->1,"type"->"text","question"->"I pianeti fanno luce. sì o no?","answer"->"no","hints"->{"Riflettono la luce","Non sono stelle","Non bruciano come il Sole"}|>,
<|"level"->1,"type"->"text","question"->"Le stelle fanno luce. sì o no?","answer"->"sì","hints"->{"Producono energia","Reazioni nucleari","Brillano di luce propria"}|>,
<|"level"->1,"type"->"text","question"->"La Terra ha vita. sì o no?","answer"->"sì","hints"->{"Presenza di acqua e atmosfera","Condizioni favorevoli","Ci siamo noi"}|>,
<|"level"->1,"type"->"text","question"->"Marte ha vita certa. sì o no?","answer"->"no","hints"->{"Non confermata","Solo ipotesi","Stiamo ancora mandando i Rover a cercare"}|>,
<|"level"->1,"type"->"text","question"->"Quanti pianeti ci sono nel sistema solare?","answer"->"otto","hints"->{"Conta da Mercurio a Nettuno","Non includere Plutone","È il numero dopo il sette"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il primo pianeta dal Sole?","answer"->"mercurio","hints"->{"Il più vicino alla stella","Piccolo e roccioso","Inizia con la lettera M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il secondo pianeta dal Sole?","answer"->"venere","hints"->{"Pianeta molto caldo","Brilla molto nel cielo","Inizia con la lettera V"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il terzo pianeta dal Sole?","answer"->"terra","hints"->{"Il pianeta dove abitiamo","L'unico con molta acqua liquida","Inizia con la lettera T"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il quarto pianeta dal Sole?","answer"->"marte","hints"->{"Il pianeta rosso","Ha due piccole lune","Inizia con la lettera M"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il quinto pianeta dal Sole?","answer"->"giove","hints"->{"Il primo dei giganti gassosi","Il più grande di tutti","Inizia con la lettera G"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il sesto pianeta dal Sole?","answer"->"saturno","hints"->{"Famoso per i suoi grandi anelli","Un gigante gassoso","Inizia con la lettera S"}|>,
<|"level"->1,"type"->"text","question"->"Qual è il settimo pianeta dal Sole?","answer"->"urano","hints"->{"Un gigante di ghiaccio azzurro","Ruota su un fianco","Inizia con la lettera U"}|>,
<|"level"->1,"type"->"text","question"->"Qual è l'ottavo pianeta dal Sole?","answer"->"nettuno","hints"->{"Il più lontano dal centro","Molto freddo e ventoso","Inizia con la lettera N"}|>
}