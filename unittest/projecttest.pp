Program ProjectTest;
{$mode objfpc}

uses Classes, TextTestRunner,
     connector, repository, searchcriteria, querybuilder, onetooneselfmodel, masterdetailmodel, manytomanymodel, jsonmodel;

begin
  // Register all tests
  querybuilder.RegisterTests;
  connector.RegisterTests;
  repository.RegisterTests;
  searchcriteria.RegisterTests;
  onetooneselfmodel.RegisterTests;
  masterdetailmodel.RegisterTests;
  manytomanymodel.RegisterTests;
  jsonmodel.RegisterTests;

  RunRegisteredTests;
end.