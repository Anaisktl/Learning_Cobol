       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculator.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-NUM1                   PIC 9(3). 
       01  WS-NUM2                   PIC 9(3).

       01  WS-NUM1-BIS               PIC Z(3).
       01  WS-NUM2-BIS               PIC Z(3).

       01  WS-CHOICE-OPERATOR        PIC X(1).

       01  WS-RESULT                 PIC -Z(3).

       01  WS-CONTINUE               PIC X(1) VALUE "O".


       PROCEDURE DIVISION.
           DISPLAY "CALCULATRICE COBOL".

      * Boucle qui exécute les instructions suivantes jusqu'à 
      * ce que la variable WS-CONTINUE soit égale à "N".
           PERFORM UNTIL WS-CONTINUE = "N"

           DISPLAY "ENTREZ UN NOMBRE"
               ACCEPT WS-NUM1
               MOVE WS-NUM1 TO WS-NUM1-BIS

           DISPLAY "ENTREZ UN OPÉRATEUR : + , - , * , / "
               ACCEPT WS-CHOICE-OPERATOR

           DISPLAY "ENTREZ UN DEUXIÈME NOMBRE"
               ACCEPT WS-NUM2
               MOVE WS-NUM2 TO WS-NUM2-BIS


           DISPLAY " CALCUL : " 
           FUNCTION TRIM(WS-NUM1-BIS) WS-CHOICE-OPERATOR 
           FUNCTION TRIM(WS-NUM2-BIS) WS-RESULT


      * La variable WS-CHOICE-OPERATOR est testée pour adapter le calcul.
           EVALUATE WS-CHOICE-OPERATOR

           WHEN "+"
               PERFORM 0100-OP-ADD-START
                  THRU 0100-OP-ADD-END

           WHEN "-"
               PERFORM 0200-OP-SUB-START
                  THRU 0200-OP-SUB-END

           WHEN "*"
               PERFORM 0300-OP-MULT-START
                  THRU 0300-OP-MULT-END

           WHEN "/"
               PERFORM 0400-OP-DIVI-START
                  THRU 0400-OP-DIVI-END
              
           WHEN OTHER
           DISPLAY "ERREUR !"

           
           END-EVALUATE

           DISPLAY "VOULEZ-VOUS FAIRE UN AUTRE CALCUL ? O/N"
           ACCEPT WS-CONTINUE
           END-PERFORM.

           STOP RUN.

    
      *** PARAGRAPHES. ***
       0100-OP-ADD-START.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
           DISPLAY "=" FUNCTION TRIM(WS-RESULT).
       0100-OP-ADD-END.
           EXIT.

       0200-OP-SUB-START.
           SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING WS-RESULT.
           DISPLAY "=" FUNCTION TRIM(WS-RESULT).
       0200-OP-SUB-END.
           EXIT.
       
       0300-OP-MULT-START.
           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.
           DISPLAY "=" FUNCTION TRIM(WS-RESULT).
       0300-OP-MULT-END.
           EXIT.

       0400-OP-DIVI-START.
           DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.
           DISPLAY "=" FUNCTION TRIM(WS-RESULT).
       0400-OP-DIVI-END.
           EXIT.


    
      
