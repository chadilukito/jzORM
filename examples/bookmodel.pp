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

unit BookModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cBookModel = class(cORMBaseModel)
    private
      const
        Field_Id    = 'id';
        Field_Title = 'title';
        Field_ISBN  = 'isbn';

      var
        fId: Integer;
        fTitle: String;
        fISBN: String;

    protected
      procedure setId(setvar: Integer);
      procedure setTitle(setvar: String);
      procedure setISBN(setvar: String);

      function getId(): Integer;
      function getTitle(): String;
      function getISBN(): String;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property Title: String read getTitle write setTitle;
      property ISBN: String read getISBN write setISBN;
  end;

implementation

procedure cBookModel.initField();
  begin
    addField(Field_Id, mdtInteger, true, true);
    addField(Field_Title, mdtString);
    addField(Field_ISBN, mdtString);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Title, @setTitle, @getTitle);
    setFieldCallback(Field_ISBN, @setISBN, @getISBN);
  end;

procedure cBookModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cBookModel.setTitle(setvar: String);
  begin
    fTitle := setvar;
    fieldUpdated(Field_Title);
  end;

procedure cBookModel.setISBN(setvar: String);
  begin
    fISBN := setvar;
    fieldUpdated(Field_ISBN);
  end;

function cBookModel.getId(): Integer;
  begin
    result := fId;
  end;

function cBookModel.getTitle(): String;
  begin
    result := fTitle;
  end;

function cBookModel.getISBN(): String;
  begin
    result := fISBN;
  end;

end.