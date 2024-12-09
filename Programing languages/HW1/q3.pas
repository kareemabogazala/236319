program q3;

uses
  SysUtils;

type
  CourseType = (REG, LAB, SEM);

  Course = record
    m_ID: Integer;
    m_Name: string; 
    case m_Type: CourseType of
      REG: (WeeklyHours: Integer);
      LAB: (LabSupervisor: string; Room: string); 
      SEM: (StartTime: string; DoneTime: string); 
  end;

var
  Courses: array[1000..1100] of Course;

function DataToCourse(iID: Integer; iName: string; iType: string; iTypeSpecificData: array of string): Course;
var
  temp: Course;
begin
  temp.m_ID := iID;
  temp.m_Name := iName;
  if iType = 'REG' then
  begin
    temp.m_Type := REG;
    temp.WeeklyHours := StrToInt(iTypeSpecificData[0]);
  end
  else if iType = 'LAB' then
  begin
    temp.m_Type := LAB;
    temp.LabSupervisor := iTypeSpecificData[0];
    temp.Room := iTypeSpecificData[1];
  end
  else if iType = 'SEM' then
  begin
    temp.m_Type := SEM;
    temp.StartTime := iTypeSpecificData[0];
    temp.DoneTime := iTypeSpecificData[1];
  end;
  DataToCourse := temp;
end;

procedure PrintCourse(course: Course);
begin
  case course.m_Type of
    REG:
      begin
        writeln('REG');
        writeln(course.m_ID);
        writeln(course.m_Name);
        writeln(course.WeeklyHours);
      end;
    LAB:
      begin
        writeln('LAB');
        writeln(course.m_ID);
        writeln(course.m_Name);
        writeln(course.LabSupervisor);
        writeln(course.Room);
      end;
    SEM:
      begin
        writeln('SEM');
        writeln(course.m_ID);
        writeln(course.m_Name);
        writeln(course.StartTime);
        writeln(course.DoneTime);
      end;
  end;
end;

var
  data: array[1..5] of string;
  i, print_id: Integer;
  input: string;

begin
  ReadLn(input);
  while not (CompareStr(input, 'END') = 0) do
  begin
    if CompareStr(input, 'ADD') = 0 then
    begin
      ReadLn(data[1]);
      ReadLn(data[2]);
      ReadLn(data[3]);
      if CompareStr(data[1], 'REG') = 0 then
      begin
        ReadLn(data[4]);
        Courses[StrToInt(data[2])] := DataToCourse(StrToInt(data[2]), data[3], data[1], [data[4]]);
      end
      else if CompareStr(data[1], 'LAB') = 0 then
      begin
        ReadLn(data[4]);
        ReadLn(data[5]);
        Courses[StrToInt(data[2])] := DataToCourse(StrToInt(data[2]), data[3], data[1], [data[4], data[5]]);
      end
      else if CompareStr(data[1], 'SEM') = 0 then
      begin
        ReadLn(data[4]);
        ReadLn(data[5]);
        Courses[StrToInt(data[2])] := DataToCourse(StrToInt(data[2]), data[3], data[1], [data[4], data[5]]);
      end;
    end
    else if CompareStr(input, 'PRINT') = 0 then
    begin
      ReadLn(print_id);
      PrintCourse(Courses[print_id]);
    end;
    ReadLn(input);
 end;
end.