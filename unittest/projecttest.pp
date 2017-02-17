Program ProjectTest;
{$mode objfpc}
{$modeswitch UnicodeStrings}
{$H+}
 
uses Classes, TextTestRunner,
     connector, repository, querybuilder;
 
 
begin
  // Register all tests
  querybuilder.RegisterTests;
  connector.RegisterTests;
  repository.RegisterTests;
 
  RunRegisteredTests;
end.