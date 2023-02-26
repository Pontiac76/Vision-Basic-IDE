unit SimpleSQLite3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLite3, SQLDB;

function OpenDB (const dbName: string; out DBObj: TSQLite3Connection): boolean;
procedure CloseDB (var DBObj: tSqlite3Connection);

implementation

function OpenDB (const dbName: string; out DBObj: TSQLite3Connection): boolean;
var
  sqlite3: TSQLite3Connection;
  sl: TStrings;
begin
  // create components
  sqlite3 := TSQLite3Connection.Create(nil);
  sqlite3.Transaction := TSQLTransaction.Create(nil);

  // setup db
  sqlite3.DatabaseName := dbName;
  sqlite3.HostName := 'localhost';
  sqlite3.CharSet := 'UTF8';

  // open db
  try
    sqlite3.Open;
    DBObj := sqlite3;
    Result := sqlite3.Connected;
  except
    on E: Exception do begin
      sqlite3.Close;
      Result := False;
    end;
  end;
end;

procedure CloseDB (var DBObj: TSQLite3Connection);
begin
  // disconnect
  if Assigned(DBObj) then begin
    if DBObj.Connected then begin
      TSQLTransaction(DBObj.Transaction).Commit;
      DBObj.Close;
    end;

    // release
    TSQLTransaction(DBObj.Transaction).Free;
    DBObj.Free;
  end;
end;


end.
