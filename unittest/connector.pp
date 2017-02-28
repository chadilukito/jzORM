unit connector;
{$modeswitch UnicodeStrings}
{H+}

interface

uses TestFramework, jzorm.mysqlconnector;

type  
  cConnectorTest = class(TTestCase)
    private
      function createConnection(): cORMMySqlConnector;

    published
      procedure Test_Connection;
      //procedure TestDBPool_UseInit;
      //procedure TestDBPool_MaxLimit;
      //procedure TestDBPool_Release_n_Reuse;
      //procedure TestDBPool_GetWithSmallTimeout;
  end;


procedure RegisterTests;

implementation

uses sysutils, typinfo;
  
procedure RegisterTests;
begin
  TestFramework.RegisterTest('jzORM Test Suite', cConnectorTest.Suite);
end;

procedure cConnectorTest.Test_Connection;
  var
     conn: cORMMySqlConnector;

  begin
    conn := createConnection();
    CheckEquals(true, conn.Connected, 'Failed - Connected Connector');

    FreeAndNil(conn);
  end;

function cConnectorTest.createConnection(): cORMMySqlConnector;
  begin
    result := cORMMySqlConnector.create('127.0.0.1', 3306, 'root', '', 'test');
  end;

end.