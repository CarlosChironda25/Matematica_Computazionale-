(* ::Package:: *)

(* ::Package:: *)
(**)


BeginPackage["OmniCosmos`"]

(* ========================================================================= *)
(* 1. DICHIARAZIONE DELLE FUNZIONI PUBBLICHE (USAGE)                         *)
(* ========================================================================= *)

getExercise::usage = "getExercise[lvl] estrae un esercizio dal database in base al livello."
getHints::usage = "getHints[ex] restituisce la lista degli aiuti per l'esercizio fornito."
checkAnswer::usage = "checkAnswer[ex, ans] verifica se la risposta inserita \[EGrave] corretta."
LaunchOmniCosmos::usage = "LaunchOmniCosmos[] lancia l'interfaccia grafica unificata dell'accademia spaziale."

Begin["`Private`"]

(* ========================================================================= *)
(* 2. LOGICA DI SUPPORTO                                                     *)
(* ========================================================================= *)

getExercise[lvl_] := Module[{filtered},
  (* Cerca di filtrare dal database globale exerciseDB se esistente *)
  filtered = If[ValueQ["Global`exerciseDB"] || ValueQ["OmniCosmos`Private`exerciseDB"], 
    Select[exerciseDB, #["level"] == lvl &], 
    {}
  ];
  
  If[Length[filtered] > 0,
    RandomChoice[filtered],
    (* Fallback di sicurezza se il database \[EGrave] vuoto o non ancora caricato *)
    <|"level" -> lvl, "type" -> "text", "question" -> "Esercizio di test: Qual \[EGrave] il pianeta azzurro?", "answer" -> "terra", "hints" -> {"Inizia con T", "Ci viviamo noi!"}|>
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

LaunchOmniCosmos[] := DynamicModule[
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
        Style["\|01f9e0 Accademia Spaziale OmniCosmos", 22, Bold, Darker[Blue]],
        Style["Esercizi e sfide di astronomia per ragazzi", 12, Italic, GrayLevel[0.3]],
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
         Row[{Style["\|01f3c6 Stelle Guadagnate: ", Bold, 14], Style[Dynamic[score], 16, Bold, RGBColor[0.9, 0.6, 0]]}],
         Background -> LightYellow, FrameMargins -> 8
        ],
        Spacer[15]
      }, Alignment -> Center],
      
      (* 3. Sezione SEED e Controllo Generazione *)
      Grid[{
        {
         Style["Codice Esercizio (Seed): ", 12, Bold], 
         InputField[Dynamic[userSeed], Number, FieldSize -> 6],
         Spacer[5],
         Button["Genera da Codice",
           SeedRandom[userSeed];
           currentExercise = getExercise[level];
           userAnswer = ""; feedback = "Domanda caricata dal codice " <> ToString[userSeed] <> ".";
           alreadyAnswered = False; showAnswer = False; hintIndex = 0;,
           ImageSize -> {130, 25}, BaseStyle -> 11
         ],
         Spacer[5],
         Button["Domanda Casuale",
           SeedRandom[];
           currentExercise = getExercise[level];
           userAnswer = ""; feedback = "";
           alreadyAnswered = False; showAnswer = False; hintIndex = 0;,
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
        Background -> LightBlue, ImageSize -> {500, Automatic}, Alignment -> Center
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
        Button["\|01f5d1\:fe0f Cancella Scritta", userAnswer = ""; feedback = "Campo pulito.";, ImageSize -> {150, 22}, BaseStyle -> 10]
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
