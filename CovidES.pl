% Amde Beaumont 1507124

:- use_module(library(pce)).

:- dynamic stats/2.
:- dynamic symptom_of/1.
:- dynamic increase_risk/1.

stats(0,0).
illness(covid).

increase_risk(diabetes, covid).
increase_risk(hypertension, covid).
increase_risk(asthma, covid).
increase_risk(sinusitus, covid).
increase_risk(covidcontact, covid).

symptom_of(cough, covid).
symptom_of(highfever, covid).
symptom_of(notaste, covid).
symptom_of(nosmell, covid).

% main function
run:-
    new(H1, dialog("Ministry of Health COVID-19 Expert System")),
    send(H1, append, new(label)),
    send(H1, append, new(L1, label)),
    send(L1, append, "Welcome to MOH COVID-19 Expert System"),
    send(H1, append, new(L2, label)),
    send(L2, append, "Choose an option below."),
    send(H1, append, new(label)),
    send(H1, append, button("Diagnose", message(@prolog,diagnosis))),
    send(H1, append, button("Add Risk Factor", message(@prolog, add_risk))),
    send(H1, append, button("Add Symptom", message(@prolog, add_symptom))),
    send(H1, append, button("Statistics", message(@prolog,view_stat))),
    send(H1, append, button("Database", message(@prolog,file_read))),
    send(H1, open).

% Adds new Risk Factor to facts
add_risk:-
    new(H2,dialog("Add Risk Factor")),
    send(H2, append, new(label)),
    send(H2, append, new(Risk, text_item(risk_factor))),
    send(H2, append, button(accept, message(@prolog, save_risk, Risk?selection))),
    send(H2, open).

save_risk(R):-
    assert(increase_risk(R, covid)), nl, nl, write("Added New Risk Factor "), write(R).

% Adds new symptom to facts
add_symptom:-
    new(H3,dialog("Add Symptom")),
    send(H3, append, new(label)),
    send(H3, append, new(Symptom, text_item(symptom))),
    send(H3, append, button(accept, message(@prolog, save_symptom, Symptom?selection))),
    send(H3, open).

save_symptom(S):-
    assert(symptom_of(S, covid)), nl, nl, write("Added New Symptom "), write(S).

% Accepts input to make diagnosis    
diagnosis:-
    new(H4, dialog("COVID-19 Diagnosis")),
    send(H4, append,new(label)),
    send(H4, append, new(PatientID, int_item(patient))),
    send(H4, append, new(Age, int_item(age, low := 1, high := 130))),
    send(H4, append, new(Gender, menu(gender,marked))),
    send(H4, append, new(TempC, int_item("Temperature(C): "))),
    send(H4, append, new(Dizziness, menu("Dizziness: ", marked))),
    send(H4, append, new(Faint, menu("Fainting: ", marked))),
    send(H4, append, new(Blur, menu("Blurry Vision: ", marked))),
    send(H4, append, new(Taste, menu("Loss of Taste: ", marked))),
    send(H4, append, new(Smell, menu("Loss of Smell: ", marked))),
    send(H4, append, new(Symptom, text_item(symptom))),

    send(PatientID, type, int),
    send(Age, type, int),
    send(TempC, type, int),
    send(Gender, append, male),         send(Gender, append, female),
    send(Dizziness, append, yes),       send(Dizziness, append, no),
    send(Faint, append, yes),           send(Faint, append, no),
    send(Blur, append, yes),            send(Blur, append, no),
    send(Taste, append, yes),            send(Taste, append, no),
    send(Smell, append, yes),            send(Smell, append, no),

    send(H4, append, button("Run Diagnostic", message(@prolog, diagnose,
    PatientID?selection,        Age?selection,
    Gender?selection,           TempC?selection,
    Dizziness?selection,        Faint?selection,
    Blur?selection,             Taste?selection,
    Smell?selection,            Symptom?selection))),

    send(H4,append,button("Exit",message(H4, destroy))),
    send(H4, open).


% Displays Diagnosis info, converts Celcius to Fahrenheit, Calculates Risk
diagnose(PatientID, Age, Gender, TempC, Dizziness, Faint, Blur, Taste, Smell, Symptom):-
    new(H7, dialog("Final Diagnosis")),
    send(H7, append, new(Lbl1,label)), send(Lbl1, append, "Patient ID: "),
    send(H7, append, new(Lbl01,label)), send(Lbl01, append, PatientID),

    send(H7, append, new(Lbl2,label)), send(Lbl2, append, "Age: "),
    send(H7, append, new(Lbl02,label)), send(Lbl02, append, Age),

    send(H7, append, new(Lbl3,label)), send(Lbl3, append, "Gender: "),
    send(H7, append, new(Lbl03,label)), send(Lbl03, append, Gender),

    send(H7, append, new(Lbl4,label)), send(Lbl4, append, "Temperature(C): "),
    send(H7, append, new(Lbl04,label)), send(Lbl04, append, TempC),

    TempF is ((TempC * (9/5)) + 32),

    send(H7, append, new(Lbl5,label)), send(Lbl5, append, "Temperature(F): "),
    send(H7, append, new(Lbl05,label)), send(Lbl05, append, TempF),

    (Age > 60 -> R1 is 1; R1 is 0),
    (TempF > 99 -> R2 is 1; R2 is 0),
    (Dizziness == 'yes' -> R3 is 1; R3 is 0),
    (Faint == 'yes' -> R4 is 1; R4 is 0),
    (Blur == 'yes' -> R5 is 1; R5 is 0),
    (Taste == 'yes' -> R6 is 1; R6 is 0),
    (Smell == 'yes' -> R7 is 1; R7 is 0),
    (symptom_of(Symptom) -> R8 is 1; R8 is 0),

    Risk_value is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8,

    send(H7, append, new(Lbl6,label)),

    (Risk_value >= 5 -> send(Lbl6, append, "High Risk Of COVID-19 
    Recommendations: Stay Home, Quarantine, Wear a Mask, Sanitize often, Seek Medical Assistance"), Case is 1;
    send(Lbl6, append, "Low Risk Of COVID-19
    Recommendations: Wear a mask, Sanitize often, Social Distance"), Case is 0),

    updstats(Case),

    file_write(PatientID, Age, Gender, TempF, Dizziness, Faint, Blur, Taste, Smell, Symptom),
    send(H7, append, button("Report Case", message(@prolog, report))),

    (Dizziness == 'yes' -> send(H7, append, button("Blood Pressure Check", message(@prolog, bp_check)))),
    send(H7, open).

% Accepts Blood Pressure Data
bp_check:-
    new(H5, dialog("Blood Pressure Check")),
    send(H5, append, new(label)),
    send(H5, append, new(BP, menu("Blood Pressure",marked))),
    send(H5, append, new(label)),
    send(BP, layout, orientation:=vertical),
    send(BP, append, systolic_under_90_Diastolic_under_60),
    send(BP, append, systolic_over_90_Diastolic_over_60),
    send(H5, append, button("Get Results", message(@prolog, bp_results, BP?selection))),
    send(H5, open).

% Returns Blood Pressure Information
bp_results(BP):-
    new(H6, dialog("Blood Pressure Results")),
    send(H6, append, new(label)),
    send(H6, append, new(Lbl1, label)),
    (BP == "Systolic_under_90_Diastolic_under_60" -> send(Lbl1, append, "Low Blood Pressure"),
    BP == "Systolic_over_90_Diastolic_over_60" -> send(Lbl1, append, "Blood Pressure is not low"), fail),
    send(H6,open).

% Updates User and cases counter
updstats(X):-
    stats(TotalCount, HrCount),
    New_Total is TotalCount + 1,
    New_HR is HrCount + X,
    retractall(stats(_,_)),
    assert(stats(New_Total,New_HR)).

% User and Cases counter
view_stat:-
    stats(TotalCount, HrCount),nl,
    write("Total Persons: "), write(TotalCount),nl,
    write("High Risk Count: "), write(HrCount), fail,
    new(H10, dialog("Statistics")),
    send(H10, append, new(label)),
    send(H10, append, new(Lbl1, label)),
    send(Lbl1, append, "Total Persons: "),
    send(H10, append, new(Lbl01,label)), 
    send(Lbl01, append, TotalCount),
    send(H10, append, new(Lbl2, label)),
    send(Lbl2, append, "High Risk Count: "),
    send(H10, append, new(Lbl02,label)), 
    send(Lbl02, append, HrCount),
    send(H10, open).
    
% Return for reported cases
report:-
    write("Case Reported "),
    new(H8, dialog("Case Report")), 
    send(H8, append, new(label)),
    send(H8, append, new(Lbl7, label)),
    send(Lbl7, append,"Case Reported to Minitry Of Health"),
    send(H8, open).

% Read from File
file_read:-
    open("coviddb.txt", read, Str),
    get_char(Str, Output),
    process_stream(Output, Str),
    close(Str).

% File Handling
process_stream(end_of_file,_):-!.

process_stream(Char,Str):-write(Char), get_char(Str,Char2), process_stream(Char2,Str).

% Write to File
file_write(PatientID, Age, Gender, TempF, Dizziness, Faint, Blur, Taste, Smell, Symptom):-
    open("coviddb.txt", append, Stream),

    nl(Stream),
    write(Stream, "Patient ID: "),          write(Stream,PatientID),        write(Stream, "  "),
    write(Stream, "Age: "),                 write(Stream,Age),              write(Stream, "  "),
    write(Stream, "Gender: "),              write(Stream,Gender),           write(Stream, "  "),
    write(Stream, "Temperature(F): "),      write(Stream,TempF),            write(Stream, "  "),
    write(Stream, "Dizziness: "),           write(Stream,Dizziness),        write(Stream, "  "),
    write(Stream, "Faint: "),               write(Stream,Faint),            write(Stream, "  "),
    write(Stream, "Blur: "),                write(Stream,Blur),             write(Stream, "  "),
    write(Stream, "Taste: "),               write(Stream,Taste),            write(Stream, "  "),
    write(Stream, "Smell: "),               write(Stream,Smell),            write(Stream, "  "),
    write(Stream, "Symptoms: "),            write(Stream,Symptom),          write(Stream, "  "),
    close(Stream),
    new(H9, dialog("Database")),
    send(H9, append, new(Lbl8, label)),
    send(Lbl8, append, "Data saved Successfully").



