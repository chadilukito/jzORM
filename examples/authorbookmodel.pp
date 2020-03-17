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

unit AuthorBookModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cAuthorBookModel = class(cORMBaseModel)
    private
      const
        Field_Id     = 'id';
        Field_Author = 'authorid';
        Field_Book   = 'bookid';

      var
        fId: Integer;
        fAuthorId: Integer;
        fBookId: Integer;

    protected
      procedure setId(setvar: Integer);
      procedure setAuthorId(setvar: Integer);
      procedure setBookId(setvar: Integer);

      function getId(): Integer;
      function getAuthorId(): Integer;
      function getBookId(): Integer;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property AuthorId: Integer read getAuthorId write setAuthorId;
      property BookId: Integer read getBookId write setBookId;
  end;

implementation

uses bookmodel, authormodel;

procedure cAuthorBookModel.initField();
  begin
    addField(Field_Id, mdtInteger, true, true);
    addField(Field_Author, mdtInteger);
    addField(Field_Book, mdtInteger);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Author, @setAuthorId, @getAuthorId);
    setFieldCallback(Field_Book, @setBookId, @getBookId);
  end;

procedure cAuthorBookModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cAuthorBookModel.setAuthorId(setvar: Integer);
  begin
    fAuthorId := setvar;
    fieldUpdated(Field_Author);
  end;

procedure cAuthorBookModel.setBookId(setvar: Integer);
  begin
    fBookId := setvar;
    fieldUpdated(Field_Book);
  end;

function cAuthorBookModel.getId(): Integer;
  begin
    result := fId;
  end;

function cAuthorBookModel.getAuthorId(): Integer;
  begin
    result := fAuthorId;
  end;

function cAuthorBookModel.getBookId(): Integer;
  begin
    result := fBookId;
  end;

end.