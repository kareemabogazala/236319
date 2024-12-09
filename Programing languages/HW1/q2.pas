program q2;
var
    s1,s2,s3: string;
    hist1,hist2,hist3: array[1..26] of integer;  
    i, index: integer;
    isValid1,isValid2,isValid: boolean;
    
begin
    ReadLn(s1);
    ReadLn(s2);
    ReadLn(s3);

    for i := 1 to 26 do
    begin
        hist1[i] := 0;
        hist2[i] := 0;
        hist3[i] := 0;
    end;

    
    for i := 1 to Length(s1) do
    begin
        index := Ord(s1[i]) - Ord('a') + 1;  
        if (index >= 1) and (index <= 26) then
            hist1[index] := hist1[index] + 1;
    end;
     for i := 1 to Length(s2) do
    begin
        index := Ord(s2[i]) - Ord('a') + 1;  
        if (index >= 1) and (index <= 26) then
            hist2[index] := hist2[index] + 1;
    end;
     for i := 1 to Length(s3) do
    begin
        index := Ord(s3[i]) - Ord('a') + 1;  
        if (index >= 1) and (index <= 26) then
            hist3[index] := hist3[index] + 1;
    end;
    isValid1 := False;
    isValid2 := False;
    isValid := True;
    for i := 1 to 26 do
    begin
        if hist3[i] > 0 then 
            begin
                if (hist2[i] = 0) and (hist1[i] = 0) then isValid := False;
                if (hist2[i] > 0) and (hist1[i] > 0) then isValid := False;
                if(hist1[i] > 0 ) then isValid1 := True;
                if(hist2[i] > 0 ) then isValid2 := True;
            end;
    end;
    if(isValid1) and isValid2 then else isValid := False;
    if isValid then WriteLn('True') else 
    begin
    WriteLn('FALSE');
    for i := 1 to 26 do
    begin
    if hist1[i]+hist2[i]+hist3[i] > 0 then
        WriteLn(Chr(i + Ord('a') - 1), ' ', hist1[i]+hist2[i]+hist3[i]);
    end;
 end;
end.
