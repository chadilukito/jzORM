unit repository;
{$modeswitch UnicodeStrings}
{H+}

interface

uses TestFramework, jzorm.mysqlconnector;

type
  cRepositoryTest = class(TTestCase)
    private
      function createConnection(): cORMMySqlConnector;

    published
      //procedure Test_Create;
      procedure Test_Delete_2;
      procedure Test_Empty;
      procedure Test_Insert_1;
      procedure Test_Insert_2;
      procedure Test_Insert_3;
      procedure Test_Update;
      procedure Test_Delete_1;
      procedure Test_Delete_3;
      procedure Test_FindAll;
      procedure Test_FindBy_1;
      procedure Test_FindOneBy_1;
      procedure Test_FindOneBy_2;
      procedure Test_Find_Empty;
      procedure Test_Count_1;
      procedure Test_Count_2;
      procedure Test_BOF_EOF;
      procedure Test_Nil_Collection;
      procedure Test_Aggregate_1;

      procedure Test_RawSQLReader;
      procedure Test_RawSQLWriter;
  end;


procedure RegisterTests;

implementation

uses classes, sysutils, typinfo,
     jzorm.baserepository, jzorm.basemodel, jzorm.modelcollection, jzorm.rawrepository,
     customermodel;

procedure RegisterTests;
begin
  TestFramework.RegisterTest('jzORM Test Suite', cRepositoryTest.Suite);
end;

{procedure cRepositoryTest.Test_Create;
  begin
  end;}

procedure cRepositoryTest.Test_Delete_2;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer', true);
    CheckEquals(true, repo.deleteAll(), 'Failed - Delete All');

    FreeAndNil(repo);
  end;

procedure cRepositoryTest.Test_Empty;
  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     fAll: specialize cORMModelCollection<cCustomerModel>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer', true);
    fAll := repo.findAll();
    CheckEquals(true, fAll.IsEmpty, 'Failed - Find All - Is Empty');

    FreeAndNil(fAll);
    FreeAndNil(repo);
  end;

procedure cRepositoryTest.Test_FindAll;
  var
    cust: cCustomerModel;
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    ColumnLimit: TStringList;
    I, idx: Integer;
    fAll: specialize cORMModelCollection<cCustomerModel>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    cust := repo.findAll().First;
    CheckEquals(11, cust.Age, 'Failed - Find All');

    fAll := repo.findAll(cOrderByField.Create('Age'));
    CheckEquals(10, fAll.First.Age, 'Failed - Find All - OrderBy');
    FreeAndNil(fAll);

    ColumnLimit := TStringList.Create();
    ColumnLimit.Add('Age');
    cust := repo.findAll(cOrderByField.Create('Age'), ColumnLimit).First;
    idx := cust.searchFieldByName('Age');
    for I := 0 to Length(cust.Fields)-1 do
      if (I <> idx) then
        CheckEquals(false, cust.Fields[I].valueSet, 'Failed - Find All - Only Age Column Selected');

    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_FindBy_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    arrmcust: specialize cORMModelCollection<cCustomerModel>;
    mcust: cCustomerModel;
    criteria: specialize TArray<cSearchCriteria>;
    search: cSearchField;
    cnt: Word;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    search := cSearchField.Create('Age', TSearchFieldOperator.sfoIn, 23);
    search.addValue(25);
    search.addValue(55);
    search.addValue(43);

    SetLength(criteria, 2);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Address', TSearchFieldOperator.sfoLike, '%c10'), scoOr);
    criteria[1] := cSearchCriteria.Create(1, search);

    arrmcust := repo.findBy(criteria, cOrderByField.Create('id'));

    cnt := 0;
    for mcust in arrmcust do inc(cnt);
    CheckEquals(5, cnt, 'Failed - FindBy 1');

    cnt := 0;
    for mcust in arrmcust do
      begin
        if (cnt = 0) then CheckEquals(19, mcust.age, 'Failed - FindBy 1 - Check each 1');
        if (cnt = 1) then CheckEquals('cm14', mcust.name, 'Failed - FindBy 1 - Check each 2');
        if (cnt = 4) then CheckEquals('customer 46', mcust.info, 'Failed - FindBy 1 - Check each 4');
        inc(cnt);
      end;

    {mcust := arrmcust.first;
    mcust.output();
    mcust := arrmcust.next;
    mcust.output();
    mcust := arrmcust.next;
    mcust.output();
    mcust := arrmcust.next;
    mcust.output();
    mcust := arrmcust.next;
    mcust.output();}

    FreeAndNil(arrmcust);
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_FindOneBy_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    mcust := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldOperator.sfoGreaterThanEqual, 33)), cOrderByField.Create('Age'));

    CheckEquals(33, mcust.Age, 'Failed - Find One By 1');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_FindOneBy_2;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;
    criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 2);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Address', TSearchFieldOperator.sfoLike, '%c10'), scoAnd);
    criteria[1] := cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldOperator.sfoEqual, 19));

    mcust := repo.findOneBy(criteria);

    CheckEquals(19, mcust.Age, 'Failed - Find One By 2');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Find_Empty;
   var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    collmcust: specialize cORMModelCollection<cCustomerModel>;
    mcust: cCustomerModel;
    criteria: specialize TArray<cSearchCriteria>;
    search: cSearchField;
    cnt: Word;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    search := cSearchField.Create('Age', TSearchFieldOperator.sfoIn, -1);
    search.addValue(-2);

    SetLength(criteria, 2);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Address', TSearchFieldOperator.sfoLike, '%c-10'), scoOr);
    criteria[1] := cSearchCriteria.Create(1, search);

    collmcust := repo.findBy(criteria, cOrderByField.Create('id'));

    cnt := 0;
    for mcust in collmcust do inc(cnt);
    CheckEquals(0, cnt, 'Failed - Find Empty');

    FreeAndNil(collmcust);
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Insert_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    mcust := cCustomerModel.Create();
    mcust.name := 'c1';
    mcust.address := 'address c1';
    mcust.age := 11;
    mcust.info := 'customer 1';

    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    CheckEquals(true, repo.insert(mcust), 'Failed - Insert 1');
    CheckNotEquals(-1, mcust.id, 'Failed - Insert - Auto Increment');
    writeln('Customer ID: '+IntToStr(mcust.id));

    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Insert_2;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;
    arrmcust: specialize TArray<cCustomerModel>;
    I: Integer;

  begin
    SetLength(arrmcust, 100);
    for I := 0 to Length(arrmcust)-1 do
      begin
        mcust := cCustomerModel.Create();
        mcust.name := 'cm'+IntToStr(I+1);
        mcust.address := 'address c'+IntToStr(I+1);
        mcust.age := 10+I;
        mcust.info := 'customer '+IntToStr(I+1);

        arrmcust[I] := mcust;
      end;

    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    CheckEquals(true, repo.insert(arrmcust), 'Failed - Insert 2');

    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Insert_3;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    mcust := cCustomerModel.Create();
    mcust.id := 58; //should be duplicate with existing one from Insert_2
    mcust.name := 'c1';
    mcust.address := 'address c1';
    mcust.age := 11;
    mcust.info := 'customer 1';

    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    CheckEquals(true, repo.insert(mcust, insIgnoreOnDuplicate), 'Failed - Insert 3');
    CheckEquals(58, mcust.id, 'Failed - Insert - Auto Increment 2');
    writeln('Customer ID: '+IntToStr(mcust.id));

    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Update;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    mcust := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldOperator.sfoEqual, 94)), cOrderByField.Create('Age'));
    mcust.Info := mcust.Info + ' - updated';
    //mcust.output();

    CheckEquals(true, repo.update(mcust), 'Failed - Update');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Delete_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');
    mcust := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldOperator.sfoEqual, 15)), cOrderByField.Create('Age'));

    CheckEquals(true, repo.delete(mcust), 'Failed - Delete 1');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Delete_3;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    mcust: cCustomerModel;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    mcust := cCustomerModel.Create();
    mcust.name := 'c1 delete';
    mcust.address := 'address c1 delete';
    mcust.age := 11;
    mcust.info := 'customer 1 delete';

    CheckEquals(true, repo.insert(mcust), 'Failed - Delete 3 - Insert');
    CheckEquals(true, repo.delete(mcust), 'Failed - Delete 3 - Delete');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Count_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    CheckEquals(100, repo.countAll(), 'Failed - Count 1');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Count_2;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    CheckEquals(25, repo.countFor(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldOperator.sfoLessThan, 35))), 'Failed - Count 2');
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_BOF_EOF;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    arrmcust: specialize cORMModelCollection<cCustomerModel>;
    criteria: specialize TArray<cSearchCriteria>;
    search: cSearchField;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    search := cSearchField.Create('Age', TSearchFieldOperator.sfoIn, 23);
    SetLength(criteria, 1);
    criteria[0] := cSearchCriteria.Create(1, search);

    arrmcust := repo.findBy(criteria, cOrderByField.Create('id'));

    CheckEquals(true, arrmcust.isBOF, 'Failed - BOF 1');
    CheckEquals(false, arrmcust.isEOF, 'Failed - EOF 1');
    arrmcust.next;
    CheckEquals(false, arrmcust.isBOF, 'Failed - BOF 2');
    CheckEquals(true, arrmcust.isEOF, 'Failed - EOF 2');

    FreeAndNil(arrmcust);
    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_Nil_Collection;
  var
     arrmcust: specialize cORMModelCollection<cCustomerModel>;
     mcust: cCustomerModel;
     cnt: Byte;

  begin
    arrmcust := (specialize cORMModelCollection<cCustomerModel>).Create(nil);
    cnt := 0;
    for mcust in arrmcust do inc(cnt);

    CheckEquals(true, arrmcust.isEmpty, 'Failed - Nil Collection 1');
    CheckEquals(0, cnt, 'Failed - Nil Collection 2');
  end;

procedure cRepositoryTest.Test_Aggregate_1;
  var
    repo: specialize cORMRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    arrmcust: specialize cORMModelCollection<cCustomerModel>;
    mcust: cCustomerModel;
    cnt: Byte;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    arrmcust := repo.findBy(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldAggregateOperator.sfaoCount, TSearchFieldOperator.sfoGreaterThan, 1)));
    cnt := 0;
    for mcust in arrmcust do inc(cnt);

    mcust := repo.findOneBy(cSearchCriteria.Create(1, cSearchField.Create('Age', TSearchFieldAggregateOperator.sfaoCount, TSearchFieldOperator.sfoGreaterThan, 1)));

    CheckEquals(1, cnt, 'Failed - Aggregate 1 - length');
    CheckEquals(11, mcust.Age, 'Failed - Aggregate 1 - data');

    FreeAndNil(conn);
  end;

procedure cRepositoryTest.Test_RawSQLReader;
  var
    repo: specialize cORMRawRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    arrmcust: specialize cORMModelCollection<cCustomerModel>;
    arrData: TArrayDataResult;
    mcust: cCustomerModel;
    cnt: Word;

  begin
    conn := createConnection();
    repo := specialize cORMRawRepository<cCustomerModel>.Create(conn, true);

    cnt := 0;
    arrmcust := repo.readDataAsModelCollection('select * from customer');
    for mcust in arrmcust do inc(cnt);
    CheckEquals(100, cnt, 'Failed - RawSQLReader 1');

    arrData := repo.readDataAsArray('select * from customer');
    CheckEquals(5, length(arrData), 'Failed - RawSQLReader 2');
    CheckEquals(101, length(arrData[0]), 'Failed - RawSQLReader 3');

    {for cnt := 0 to 100 do
      writeln(arrData[0, cnt]+ ' - ' +arrData[1, cnt]+ ' - ' +arrData[2, cnt]+ ' - ' +arrData[3, cnt]+ ' - ' +arrData[4, cnt]);}

    FreeAndNil(arrmcust);
    FreeAndNil(repo);
  end;

procedure cRepositoryTest.Test_RawSQLWriter;
  var
    repo: specialize cORMRawRepository<cCustomerModel>;
    conn: cORMMySqlConnector;
    strList: TStringList;

  begin
    conn := createConnection();
    repo := specialize cORMRawRepository<cCustomerModel>.Create(conn, true);
    CheckEquals(true, repo.writeData('update customer set age=age*10 where name = ''cm9'''), 'Failed - RawSQLWriter 1');

    strList := TStringList.Create();
    strList.Add('update customer set age=age*5 where name = ''cm19'';');
    strList.Add('update customer set age=age*100 where name = ''cm29'';');

    CheckEquals(true, repo.writeData(strList), 'Failed - RawSQLWriter 2');
    FreeAndNil(repo);
  end;

function cRepositoryTest.createConnection(): cORMMySqlConnector;
  begin
    result := cORMMySqlConnector.create('127.0.0.1', 3306, 'root', '', 'test');
  end;

end.