(* ::Package:: *)

BeginPackage["OmniCosmos`"]

(* ========================================================================= *)
(* 1. DICHIARAZIONE DELLE FUNZIONI PUBBLICHE (USAGE)                         *)
(* ========================================================================= *)

getExercise::usage = "getExercise[lvl] estrae un esercizio dal database in base al livello."
getHints::usage = "getHints[ex] restituisce la lista degli aiuti per l'esercizio fornito."
checkAnswer::usage = "checkAnswer[ex, ans] verifica se 9 inserita \[EGrave] corretta."
LaunchExerciseCore::usage = "LaunchExerciseCore[] lancia l'interfaccia grafica unificata dell'accademia spaziale."

Begin["`Private`"]

(* ========================================================================= *)
(* 2. LOGICA DI SUPPORTO     E GENERATORE DI ESERCIZI                                                 *)
(* ========================================================================= *)

getExercise[lvl_] := Module[{db, filtered, pianetiData, pianeta, tipoDomanda, p1, p2, ans, qText, hints, pesoTerra, divisore},

	If[lvl == 3,
	
	(* Dataset con dati scientifici reali dei pianeti *)
    pianetiData = <|
      "Mercurio" -> <|"lune" -> 0,   "distanza" -> 58,   "temp" -> 167|>,
      "Venere"   -> <|"lune" -> 0,   "distanza" -> 108,  "temp" -> 464|>,
      "Terra"    -> <|"lune" -> 1,   "distanza" -> 150,  "temp" -> 15|>,
      "Marte"    -> <|"lune" -> 2,   "distanza" -> 228,  "temp" -> -65|>,
      "Giove"    -> <|"lune" -> 95,  "distanza" -> 778,  "temp" -> -110|>,
      "Saturno"  -> <|"lune" -> 146, "distanza" -> 1434, "temp" -> -140|>,
      "Urano"    -> <|"lune" -> 27,  "distanza" -> 2871, "temp" -> -195|>,
      "Nettuno"  -> <|"lune" -> 14,  "distanza" -> 4495, "temp" -> -200|>
    |>;
    tipoDomanda = RandomChoice[{"sommaLune", "differenzaTemp", "proporzioneDist"}];
    
    Switch[tipoDomanda,
      "sommaLune",
        {p1, p2} = RandomSample[Keys[pianetiData], 2];
        ans = pianetiData[p1]["lune"] + pianetiData[p2]["lune"];
        qText = "Se uniamo tutte le lune orbitanti intorno a " <> p1 <> " e tutte quelle intorno a " <> p2 <> ", quante lune otteniamo in totale?";
        hints = {"Consulta i dati dei due pianeti nel simulatore dell'applicazione.", p1 <> " ha " <> ToString[pianetiData[p1]["lune"]] <> " lune e " <> p2 <> " ne ha " <> ToString[pianetiData[p2]["lune"]] <> "."};
      ,
      "differenzaTemp",
        {p1, p2} = RandomSample[Keys[pianetiData], 2];
        ans = Abs[pianetiData[p1]["temp"] - pianetiData[p2]["temp"]];
        qText = "Qual \[EGrave] lo sbalzo termico (differenza assoluta di temperatura) tra la superficie di " <> p1 <> " (" <> ToString[pianetiData[p1]["temp"]] <> "\[Degree]C) e quella di " <> p2 <> " (" <> ToString[pianetiData[p2]["temp"]] <> "\[Degree]C)?";
        hints = {"Ricorda che la differenza tra un numero positivo e uno negativo si somma in valore assoluto!", "Devi calcolare l'operazione aritmetica: |" <> ToString[pianetiData[p1]["temp"]] <> " - (" <> ToString[pianetiData[p2]["temp"]] <> ")|."};
      ,
      "proporzioneDist",
        p1 = RandomChoice[{"Marte", "Giove", "Saturno", "Urano", "Nettuno"}];
        ans = Round[pianetiData[p1]["distanza"] / 150.0, 1];
        qText = "Sapendo che la Terra dista dal Sole circa 150 milioni di km (1 Unit\[AGrave] Astronomica), quante volte " <> p1 <> " (" <> ToString[pianetiData[p1]["distanza"]] <> " milioni di km) \[EGrave] pi\[UGrave] lontano dal Sole rispetto alla Terra? (Arrotonda a un solo decimale)";
        hints = {"Devi impostare una proporzione o semplicemente dividere la distanza del pianeta per 150.", "Fai il calcolo: " <> ToString[pianetiData[p1]["distanza"]] <> " / 150."};
        ];
        
        
        <|"level" -> 3, "type" -> "numeric", "question" -> qText, "answer" -> ans, "hints" -> hints|>,
	
	(*
	
	
	(*Genera numeri casuali. Il seed fisso bloccher\[AGrave] questi numeri*)
	pesoTerra = RandomInteger[{30, 90}];
	divisore = RandomChoice[{6, 3, 2}]; (* 6=Luna, 3= Marte, ecc.*)
	<|
		"level"->3,
		"type" -> "numeric",
		"question" -> "Se sulla Terra pesi " <> ToString[pesoTerra] <> "Kg, su un pianeta con gravit\[AGrave] ridotta di " <> ToString[divisore] <> " volte, quanti Kg peseresti? (Arrotonda all'intero)",
		"answer" -> Round[pesoTerra / divisore],
		"hints" -> {
			"Devi fare una divisione!",
			"Dividi " <> ToString[pesoTerra] <> " per " <> ToString[divisore] <> "."
		} 
	|>,*)
	
	db = If[NameQ["Global`exerciseDB"], Global`exerciseDB, {}];
	filtered = Select[db, #["level"] == lvl &];
	
	If[Length[filtered]>0,
		RandomChoice[filtered],
		(* fallback di sicurezza se il database non \[EGrave] ancora caricato *)
		<|"level" -> lvl, "type" -> "text", "question" -> "Errore database non trovato. Qual \[EGrave] il pianeta azzurro?", "answer" -> "terra", "hints" -> {"Controlla la directory"}|>
		]
	]
];

getHints[ex_] := If[KeyExistsQ[ex, "hints"], ex["hints"], {"Nessun aiuto disponibile per questa domanda."}];

checkAnswer[ex_, ans_] := Module[{normUser, normDB},
  If[ex["type"] === "numeric",
    Module[{numAns = ToExpression[StringReplace[ToString[ans], "," -> "."]]},
      If[NumberQ[numAns],
        (* Tolleranza per arrotondamenti adatta alle scuole medie *)
        If[Abs[numAns - ex["answer"]] <= 0.6, "perfect", False],
        False
      ]
    ],
    (* Controllo testuale normalizzato senza accenti e spazi superflui *)
    normUser = ToLowerCase[StringTrim[ToString[ans]]];
    normDB = ToLowerCase[StringTrim[ToString[ex["answer"]]]];
    If[normUser === normDB, "perfect", False]
  ]
];


(* ========================================================================= *)
(* 3. INTERFACCIA UTENTE UNIFICATA                                           *)
(* ========================================================================= *)

LaunchExerciseCore[] := DynamicModule[
 {
  userAnswer = "",          (* Input dell'alunno *)
  feedback = "",            (* Messaggi di validazione *)
  level = 1,                (* 1 = Quiz Astronomia, 3 = Logica e Numeri *)
  score = 0,                (* Contatore risposte esatte *)
  currentExercise,          (* Contiene l'associazione della domanda corrente *)
  alreadyAnswered = False,  (* Blocca i punti doppi sulla stessa domanda *)
  showAnswer = False,       (* Rivela la soluzione *)
  hintIndex = 0,            (* Indice ciclico degli aiuti *)
  userSeed = 1              (* Seed numerico obbligatorio da slide *)
 },

  (* Inizializzazione della prima domanda di default *)
  currentExercise = getExercise[1];

  (* Pannello principale centrato basato su Column *)
  Panel[
   Column[{
      
      (* 1. Intestazione *)
      Column[{
        Style["\|01f9e0 ESERCIZI STELLARIS ", 22, Bold, White],
        Style["Esercizi e sfide di astronomia per ragazzi", 12, Italic, White, Opacity[.3]],
        Spacer[10]
      }, Alignment -> Center],
      
      (* 2. Selezione livello e Punteggio *)
      Column[{
        Row[{
          Style["Seleziona la difficolt\[AGrave]: ", Bold, 13],
          SetterBar[
            Dynamic[level,
             (level = #;
              currentExercise = getExercise[level];
              userAnswer = ""; feedback = "";
              alreadyAnswered = False; showAnswer = False; hintIndex = 0;
             ) &
            ],
            {1 -> "\|01f52d Quiz Astronomia", 3 -> "\|01f9ee Logica & Numeri"}
          ]
        }],
        Spacer[5],
        Panel[
         Row[{Style["\|01f3c6 Stelle Guadagnate: ", Bold, 14], Style[Dynamic[score], 16, Bold, GrayLevel[.1]]}],
         Background -> Gray, FrameMargins -> 8
        ],
        Spacer[15]
      }, Alignment -> Center],
      
      (* 3. Sezione SEED e Controllo Generazione *)
      Grid[{
        {
         Style["Codice Esercizio (Seed): ", 12, Bold], 
         InputField[Dynamic[userSeed], Number, FieldSize -> 6],
         Spacer[5],
         Button["Genera da codice (seed) ",
           SeedRandom[userSeed]; (* Blocca la generazione sul numero inserito *)
           currentExercise = getExercise[level];
           userAnswer = "";
           feedback = "Esercizion n. " <> ToString[userSeed] <> " caricato.";
           alreadyAnswered = False;
           showAnswer = False;
           hintIndex = 0;,
           ImageSize -> {130, 25}, BaseStyle -> 11
         ],
         Spacer[5],
         Button["Domanda Casuale",
           SeedRandom[]; (* Sblocca il seed per la vera casualit\[AGrave] *)
           currentExercise = getExercise[level];
           userAnswer = "";
           feedback = "Nuova svida generata! ";
           alreadyAnswered = False;
           showAnswer = False;
           hintIndex = 0;,
           ImageSize -> {130, 25}, BaseStyle -> 11
         ]
        }
      }, Alignment -> Center],
      
      Spacer[15],
      
      (* 4. Box della Domanda *)
      Panel[
        Dynamic[
         TextCell[currentExercise["question"], "Subsubtitle", 
          Alignment -> Center, FontSize -> 15, Bold]
        ],
        Background -> Gray, ImageSize -> {500, Automatic}, Alignment -> Center
      ],
      
      Spacer[10],
      
      (* 5. Campo inserimento risposta dell'utente *)
      Column[{
        Style["Scrivi qui la tua risposta:", 11, Bold],
        InputField[
          Dynamic[userAnswer], 
          String, 
          FieldSize -> {35, 1.5},
          BaseStyle -> {FontSize -> 14, Alignment -> Center},
          Enabled -> Dynamic[!showAnswer && !alreadyAnswered]
        ],
        Spacer[5],
        Button["\|01f5d1\:fe0f Pulisci Campi", 
		  userAnswer = ""; 
		  feedback = "Interfaccia resettata.";
		  showAnswer = False;
		  alreadyAnswered = False;
		  hintIndex = 0;
		  score = 0;, 
		  ImageSize -> {150, 22}, BaseStyle -> 10
		]
      }, Alignment -> Center],
      
      Spacer[15],
      
      (* 6. Pulsanti di Interazione Principali *)
      Row[{
        Button["\|01f4a1 Chiedi Aiuto",
          Module[{hints = getHints[currentExercise], n},
            n = Length[hints];
            If[n > 0,
              hintIndex = Mod[hintIndex, n] + 1;
              feedback = "Suggerimento: " <> hints[[hintIndex]]
            ]
          ],
          ImageSize -> {120, 32}, BaseStyle -> {Bold, 12}
        ],
        Spacer[15],
        
        Button["\:2714\:fe0f Verifica",
          Module[{result = checkAnswer[currentExercise, userAnswer]},
            If[result === "perfect" && !alreadyAnswered,
              score++;
              alreadyAnswered = True;
              feedback = "\|01f389 Bravissimo! Risposta esatta!";,
              If[!alreadyAnswered, 
                feedback = "\:274c Non \[EGrave] corretto. Rileggi bene la domanda o usa un aiuto!"
              ]
            ];
          ],
          ImageSize -> {120, 32}, BaseStyle -> {Bold, 12}, Background -> LightGray
        ],
        Spacer[15],
        
        Button["\|01f4d6 Scopri Soluzione", 
          showAnswer = True; 
          feedback = "Interfaccia bloccata. Genera una nuova domanda per continuare.";, 
          ImageSize -> {140, 32}, BaseStyle -> {Bold, 12}
        ]
      }, Alignment -> Center],
      
      Spacer[10],
      
      (* 7. Area Feedback Dinamico *)
      Row[{Dynamic[Style[feedback, 13, Italic, Darker[Gray]]]}, Alignment -> Center],
      
      Spacer[5],
      
      (* 8. Mostra Soluzione se richiesto *)
      Row[{
        Dynamic[
         If[showAnswer,
          Panel[
           Style[
            "La risposta corretta era: " <> ToUpperCase[ToString[currentExercise["answer"]]], 
            14, Bold, Darker[Green]
           ],
           Background -> RGBColor[0.9, 1, 0.9]
          ],
          ""
         ]
        ]
      }, Alignment -> Center]
      
    }, Alignment -> Center],
    ImageSize -> {580, Automatic},
    Background -> DarkGray,
    FrameMargins -> 20,
    BaseStyle -> {FontFamily -> "Helvetica"}
   ]
]

End[];

EndPackage[]
