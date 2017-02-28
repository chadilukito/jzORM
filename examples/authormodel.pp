{
    Copyright (C) 2017  -  Christian Hadi L

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

unit AuthorModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cAuthorModel = class(cORMBaseModel)
    private
      const
        Field_Id      = 'id';
        Field_Name    = 'name';

      var
        fId: Integer;
        fName: String;

    protected
      procedure setId(setvar: Integer);
      procedure setName(setvar: String);

      function getId(): Integer;
      function getName(): String;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property Name: String read getName write setName;
  end;

implementation

procedure cAuthorModel.initField();
  begin
    addField(Field_Id, mdtInteger, true);
    addField(Field_Name, mdtString);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Name, @setName, @getName);
  end;

procedure cAuthorModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cAuthorModel.setName(setvar: String);
  begin
    fName := setvar;
    fieldUpdated(Field_Name);
  end;

function cAuthorModel.getId(): Integer;
  begin
    result := fId;
  end;

function cAuthorModel.getName(): String;
  begin
    result := fName;
  end;

end.