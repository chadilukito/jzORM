unit onetooneselfmodel;
{$modeswitch UnicodeStrings}
{H+}

interface

uses TestFramework, jzorm.mysqlconnector;

type  
  cOne2OneSelfModelTest = class(TTestCase)
    private
      function createConnection(): cORMMySqlConnector;

    published
      procedure Test_Empty;
      procedure Test_InsertData;
      procedure Test_GetSelf;
      procedure Test_DeleteSelf;
  end;


procedure RegisterTests;

implementation

uses sysutils, typinfo, math, misc,
     jzorm.baserepository, jzorm.basemodel, jzorm.modelcollection,
     employeemodel, employeerepository;
  
procedure RegisterTests;
begin
  TestFramework.RegisterTest('jzORM Test Suite', cOne2OneSelfModelTest.Suite);
end;

procedure cOne2OneSelfModelTest.Test_Empty;
  var
    repo: specialize cEmployeeRepository<cEmployeeModel>;
    conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    repo := specialize cEmployeeRepository<cEmployeeModel>.Create(conn, 'employee');
    CheckEquals(true, repo.deleteAll(), 'Failed - Employee truncate');
    FreeAndNil(conn);
  end;

procedure cOne2OneSelfModelTest.Test_InsertData;
  var
    repo: specialize cEmployeeRepository<cEmployeeModel>;
    conn: cORMMySqlConnector;
    memp: cEmployeeModel;
    arrmemp: specialize TArray<cEmployeeModel>;
    I: Integer;
    nextMgr: Word;

  begin
    Randomize;
    SetLength(arrmemp, 100);

    for I := 0 to Length(arrmemp)-1 do
      begin
        memp := cEmployeeModel.Create();
        memp.name := 'em'+IntToStr(I+1);
        memp.Salary := GetRandomFloat(1000, 9999.99);
        memp.Department := 'department '+IntToStr(random(10));

        arrmemp[I] := memp;
      end;

    conn := createConnection();
    repo := specialize cEmployeeRepository<cEmployeeModel>.Create(conn, 'employee');
    CheckEquals(true, repo.insert(arrmemp), 'Failed - Insert Employee');

    // reload
    I := 0;
    for memp in repo.findAll() do
      begin
        arrmemp[I] := memp;
        inc(I);
      end;

    nextMgr := ceil(arrmemp[0].ID / 5) * 5;
    for I := 0 to Length(arrmemp)-1 do
      begin
        memp := arrmemp[I];

        if (memp.ID mod 5 <> 0) then
          begin
            memp.ManagerId := nextMgr;
          end else
          begin
            nextMgr := nextMgr + 5;
          end;

        CheckEquals(true, repo.update(memp), 'Failed - Update Employee Manager');
      end;

    //CheckEquals(true, repo.update(arrmemp), 'Failed - Update Employee Manager');
    FreeAndNil(conn);
  end;

procedure cOne2OneSelfModelTest.Test_GetSelf;
  var
     repo: specialize cEmployeeRepository<cEmployeeModel>;
     conn: cORMMySqlConnector;
     memp: cEmployeeModel;
     arrmemp: specialize cORMModelCollection<cEmployeeModel>;
     I: Word;

  begin
    conn := createConnection();
    repo := specialize cEmployeeRepository<cEmployeeModel>.Create(conn, 'employee');
    memp := repo.findAll().first;
    memp := repo.getManager(memp);
    arrmemp := repo.getSubOrdinate(memp);

    CheckEquals('em5', memp.name, 'Failed - Get Employee Manager');

    I := 0;
    for memp in arrmemp do inc(I);
    CheckEquals(4, I, 'Failed - Count Manager subordinate');

    FreeAndNil(arrmemp);
    FreeAndNil(conn);
  end;

procedure cOne2OneSelfModelTest.Test_DeleteSelf;
  begin
  end;

function cOne2OneSelfModelTest.createConnection(): cORMMySqlConnector;
  begin
    result := cORMMySqlConnector.create('127.0.0.1', 3306, 'root', '', 'test');
  end;

end.