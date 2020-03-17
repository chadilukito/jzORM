unit jsonmodel;
{$modeswitch UnicodeStrings}
{H+}

interface

uses TestFramework, jzorm.mysqlconnector, jzorm.baserepository, jzorm.basemodel;

type  
  cJsonModelTest = class(TTestCase)
    private
      function createConnection(): cORMMySqlConnector;

    published
      procedure Test_Delete_All;
      procedure Test_Insert;
      procedure Test_Update;
      procedure Test_Delete;
      procedure Test_Delete_Empty_1;
      procedure Test_Delete_Empty_2;
      procedure Test_FindAll;
      procedure Test_FindBy;
      procedure Test_FindOneBy;
  end;


procedure RegisterTests;

implementation

uses sysutils, typinfo, fpjson, jsonparser, datamodel;
  
procedure RegisterTests;
begin
  TestFramework.RegisterTest('jzORM Test Suite', cJsonModelTest.Suite);
end;

procedure cJsonModelTest.Test_Delete_All;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    CheckEquals(true, repo.deleteAll(), 'Failed - Delete All');
  end;

procedure cJsonModelTest.Test_Insert;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;
     mdata: cDataModel;
     arrmdata: specialize TArray<cDataModel>;
     I: Integer;

  begin
    SetLength(arrmdata, 12);
    for I := 0 to Length(arrmdata)-3 do
      begin
        mdata := cDataModel.Create();
        mdata.DataJson := TJSONObject(GetJSON('{"Fld1" : "Hello", "Fld2" : '+IntToStr(I+1)+', "Colors" : ["Red", "Green", "Blue"]}'));

        arrmdata[I] := mdata;
      end;
    
    mdata := cDataModel.Create();
    mdata.DataJson := TJSONObject(GetJSON(''));
    arrmdata[Length(arrmdata)-2] := mdata;
    
    mdata := cDataModel.Create();
    mdata.DataJson := TJSONObject(GetJSON('{}'));
    arrmdata[Length(arrmdata)-1] := mdata;

    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    CheckEquals(true, repo.insert(arrmdata), 'Failed - Insert');

    FreeAndNil(conn);
  end;

procedure cJsonModelTest.Test_Update;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;
     mdata: cDataModel;
     jObject: TJSONObject;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    mdata := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, 3)));
    jObject := mdata.DataJson;
    jObject.Strings['Fld1'] := 'World';
    mdata.DataJson := jObject;

    CheckEquals(true, repo.update(mdata), 'Failed - Update');
    FreeAndNil(conn);
  end;

procedure cJsonModelTest.Test_Delete;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;
     mdata: cDataModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    mdata := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, 5)));

    CheckEquals(true, repo.delete(mdata), 'Failed - Delete');
    FreeAndNil(conn);
  end;
  
procedure cJsonModelTest.Test_Delete_Empty_1;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;
     mdata: cDataModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    mdata := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, 11)));

    CheckEquals(true, repo.delete(mdata), 'Failed - Delete Empty 1');
    FreeAndNil(conn);
  end;
  
procedure cJsonModelTest.Test_Delete_Empty_2;
  var
     repo: specialize cORMRepository<cDataModel>;
     conn: cORMMySqlConnector;
     mdata: cDataModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cDataModel>.Create(conn, 'datatest');
    mdata := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, 12)));

    CheckEquals(true, repo.delete(mdata), 'Failed - Delete Empty 2');
    FreeAndNil(conn);
  end;

procedure cJsonModelTest.Test_FindAll;
  begin
  end;

procedure cJsonModelTest.Test_FindBy;
  begin
  end;

procedure cJsonModelTest.Test_FindOneBy;
  begin
  end;

function cJsonModelTest.createConnection(): cORMMySqlConnector;
  begin
    result := cORMMySqlConnector.create('127.0.0.1', 3306, 'root', '', 'test');
  end;

end.