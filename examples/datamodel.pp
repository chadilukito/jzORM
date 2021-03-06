{
    Copyright (C) 2017-2020  -  Christian Hadi L

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
}

unit DataModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes, fpjson;

type
  cDataModel = class(cORMBaseModel)
    private
      const
        Field_Id       = 'id';
        Field_Json     = 'json';
        Field_LargeInt = 'largeint';

      var
        fId: Integer;
        fJson: TJSONObject;
        fLargeInt: Int64;

    protected
      procedure setId(setvar: Integer);
      procedure setJson(setvar: TJSONObject);
      procedure setLargeInt(setvar: Int64);

      function getId(): Integer;
      function getJson(): TJSONObject;
      function getLargeInt(): Int64;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property DataJson: TJSONObject read getJson write setJson;
      property LargeInteger: Int64 read getLargeInt write setLargeInt;
  end;

implementation

procedure cDataModel.initField();
  begin
    addField(Field_Id, mdtInteger, true, true);
    addField(Field_Json, mdtJson);
    addField(Field_LargeInt, mdtLargeInt);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Json, @setJson, @getJson);
    setFieldCallback(Field_LargeInt, @setLargeInt, @getLargeInt);
  end;

procedure cDataModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cDataModel.setJson(setvar: TJSONObject);
  begin
    fJson := setvar;
    fieldUpdated(Field_Json);
  end;

procedure cDataModel.setLargeInt(setvar: Int64);
  begin
    fLargeInt := setvar;
    fieldUpdated(Field_LargeInt);
  end;

function cDataModel.getId(): Integer;
  begin
    result := fId;
  end;

function cDataModel.getJson(): TJSONObject;
  begin
    result := fJson;
  end;

function cDataModel.getLargeInt(): Int64;
  begin
    result := fLargeInt;
  end;

end.