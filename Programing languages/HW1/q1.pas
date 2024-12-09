program q1;
var
    n, i, j: integer;
    line: array[1..15, 1..15] of integer;
begin
    line[1, 1] := 1;
    WriteLn(1);
    ReadLn(n);

    for i := 2 to n do
    begin
        line[i, 1] := 1;
        line[i, i] := 1;
         Write(line[i, 1]);
         Write(' ');
        for j := 2 to i - 1 do
        begin
            line[i, j] := line[i - 1, j - 1] + line[i - 1, j];
            Write(line[i, j]);
                Write(' ');
        end;
         WriteLn(line[i, i]);
 end;
end.