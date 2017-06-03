unit searchcriteria;
{$modeswitch UnicodeStrings}
{H+}

interface

uses TestFramework, jzorm.mysqlconnector;

type
  cSearchCriteriaTest = class(TTestCase)
    private
      function createConnection(): cORMMySqlConnector;

    published
      procedure Test_SingleCriteria_1;
      procedure Test_SingleCriteria_2;
      procedure Test_SameLevel_1;
      procedure Test_DiffLevel_1;
      procedure Test_DiffLevel_2;
      procedure Test_DiffLevel_3;
      procedure Test_MixLevel_1;
      procedure Test_MixLevel_2;
  end;


procedure RegisterTests;

implementation

uses classes, sysutils, typinfo, jzorm.baserepository, customermodel;

procedure RegisterTests;
begin
  TestFramework.RegisterTest('jzORM Test Suite', cSearchCriteriaTest.Suite);
end;

procedure cSearchCriteriaTest.Test_SingleCriteria_1;
  const
    CExpected = 'Column2 = :PAR01Column2';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 1);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC SingleCriteria 1');
  end;

procedure cSearchCriteriaTest.Test_SingleCriteria_2;
  const
    CExpected = 'Age IN (:PAR01Age,:PAR02Age,:PAR03Age,:PAR04Age)';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;
     search: cSearchField;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    search := cSearchField.Create('Age', TSearchFieldOperator.sfoIn, 23);
    search.addValue(25);
    search.addValue(55);
    search.addValue(43);

    SetLength(criteria, 1);
    criteria[0] := cSearchCriteria.Create(1, search);

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC SingleCriteria 2');
  end;

procedure cSearchCriteriaTest.Test_SameLevel_1;
  const
    CExpected = 'Column2 = :PAR01Column2 OR Column3 = :PAR11Column3';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 2);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoOr);
    criteria[1] := cSearchCriteria.Create(1, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Same Level 1');
  end;

procedure cSearchCriteriaTest.Test_DiffLevel_1;
  const
    CExpected = 'Column2 = :PAR01Column2 OR (Column3 = :PAR11Column3 AND Column2 = :PAR21Column2)';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 3);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoOr);
    criteria[1] := cSearchCriteria.Create(2, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[2] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Diff Level 1');
  end;

procedure cSearchCriteriaTest.Test_DiffLevel_2;
  const
    CExpected = 'Column2 = :PAR01Column2 OR (Column3 = :PAR11Column3 AND Column2 = :PAR21Column2 AND (Column5 Is Not Null OR Column3 = :PAR41Column3))';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 5);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoOr);
    criteria[1] := cSearchCriteria.Create(2, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[2] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[3] := cSearchCriteria.Create(3, cSearchField.Create('Column5', TSearchFieldOperator.sfoNotNull, ''), scoOR);
    criteria[4] := cSearchCriteria.Create(3, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Diff Level 2');
  end;

procedure cSearchCriteriaTest.Test_DiffLevel_3;
  const
    CExpected = '(Column2 = :PAR11Column2 AND Column1 <> :PAR21Column1) OR (Column3 = :PAR41Column3 AND Column2 = :PAR51Column2)';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 6);
    criteria[0] := cSearchCriteria.Create(1, nil, scoNone);
    criteria[1] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[2] := cSearchCriteria.Create(2, cSearchField.Create('Column1', TSearchFieldOperator.sfoNotEqual, ':old'));
    criteria[3] := cSearchCriteria.Create(1, nil, scoOr);
    criteria[4] := cSearchCriteria.Create(2, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[5] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Diff Level 3');
  end;

procedure cSearchCriteriaTest.Test_MixLevel_1;
  const
    CExpected = 'Column2 = :PAR01Column2 OR (Column3 = :PAR11Column3 AND Column2 = :PAR21Column2) AND Column5 <= :PAR41Column5 AND Column6 <= :PAR51Column6';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 6);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoOr);
    criteria[1] := cSearchCriteria.Create(2, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[2] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'));
    criteria[3] := cSearchCriteria.Create(1, nil, scoAnd);
    criteria[4] := cSearchCriteria.Create(1, cSearchField.Create('Column5', TSearchFieldOperator.sfoLessThanEqual, ':old'), scoAnd);
    criteria[5] := cSearchCriteria.Create(1, cSearchField.Create('Column6', TSearchFieldOperator.sfoLessThanEqual, ':old'));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Mix Level 1');
  end;

procedure cSearchCriteriaTest.Test_MixLevel_2;
  const
    CExpected = 'Column2 = :PAR01Column2 OR (Column3 = :PAR11Column3 AND Column2 = :PAR21Column2) AND Column5 <= :PAR41Column5 AND (Column6 Is Null)';

  var
     repo: specialize cORMRepository<cCustomerModel>;
     conn: cORMMySqlConnector;
     criteria: specialize TArray<cSearchCriteria>;

  begin
    conn := createConnection();
    repo := specialize cORMRepository<cCustomerModel>.Create(conn, 'customer');

    SetLength(criteria, 6);
    criteria[0] := cSearchCriteria.Create(1, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'), scoOr);
    criteria[1] := cSearchCriteria.Create(2, cSearchField.Create('Column3', TSearchFieldOperator.sfoEqual, ':old'), scoAnd);
    criteria[2] := cSearchCriteria.Create(2, cSearchField.Create('Column2', TSearchFieldOperator.sfoEqual, ':old'));
    criteria[3] := cSearchCriteria.Create(1, nil, scoAnd);
    criteria[4] := cSearchCriteria.Create(1, cSearchField.Create('Column5', TSearchFieldOperator.sfoLessThanEqual, ':old'), scoAnd);
    criteria[5] := cSearchCriteria.Create(2, cSearchField.Create('Column6', TSearchFieldOperator.sfoNull, ''));

    CheckEquals(CExpected, repo.processSearchCriteria(criteria), 'Failed - SC Mix Level 2');
  end;

function cSearchCriteriaTest.createConnection(): cORMMySqlConnector;
  begin
    result := cORMMySqlConnector.create('127.0.0.1', 3306, 'root', '', 'test');
  end;

end.