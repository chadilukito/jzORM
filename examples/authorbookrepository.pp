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

unit AuthorBookRepository;
{$modeswitch UnicodeStrings}
interface

uses jzorm.baserepository, authormodel, bookmodel, authorbookmodel, jzorm.modelcollection;

type
  generic cAuthorRepository<T: cAuthorModel> = class(specialize cORMRepository<T>)
    private
      type
        TArrMAuthorBook = specialize cORMModelCollection<cAuthorBookModel>;

    public
      function getBooks(model: T): specialize cORMModelCollection<cBookModel>;
  end;

  generic cBookRepository<T: cBookModel> = class(specialize cORMRepository<T>)
    public
      function getAuthors(model: T): specialize cORMModelCollection<cAuthorModel>;
  end;

  generic cAuthorBookRepository<T: cAuthorBookModel> = class(specialize cORMRepository<T>)
    private
      type
        TArrBook = specialize TArray<cBookModel>;
        TArrAuthor = specialize TArray<cAuthorModel>;

    public
      function getAuthorsFromBook(model: cBookModel): specialize cORMModelCollection<cAuthorModel>;
      function getBooksFromAuthor(model: cAuthorModel): specialize cORMModelCollection<cBookModel>;

      function linkAuthorBook(authormodel: cAuthorModel; bookmodel: cBookModel): Boolean; overload;
      function linkAuthorBook(authormodel: cAuthorModel; bookmodels: specialize TArray<cBookModel>): Boolean; overload;
      function linkAuthorBook(authormodels: specialize TArray<cAuthorModel>; bookmodel: cBookModel): Boolean; overload;
      function linkAuthorBook(authormodels: specialize TArray<cAuthorModel>; bookmodels: specialize TArray<cBookModel>): Boolean; overload;

      function removeLinkAuthorBook(authormodel: cAuthorModel; bookmodel: cBookModel): Boolean;
  end;

implementation

uses sysutils;

{ cAuthorRepository }

function cAuthorRepository.getBooks(model: T): specialize cORMModelCollection<cBookModel>;
  var
     repoab: specialize cAuthorBookRepository<cAuthorBookModel>;
     repob: specialize cBookRepository<cBookModel>;
     abcolls: TArrMAuthorBook;
     mab: cAuthorBookModel;
     search: cSearchField;

  begin
    repoab := specialize cAuthorBookRepository<cAuthorBookModel>.Create(ormConnect, 'book_author');
    abcolls := repoab.findBy(cSearchCriteria.Create(1, cSearchField.Create('authorid', TSearchFieldOperator.sfoEqual, model.Id)));

    search := nil;
    for mab in abcolls do
      begin
        if (not Assigned(search)) then
          search := cSearchField.Create('id', TSearchFieldOperator.sfoIn, mab.bookid)
        else
          search.addValue(mab.bookid);
      end;

    repob := specialize cBookRepository<cBookModel>.Create(ormConnect, 'book');
    result := repob.findBy(cSearchCriteria.Create(1, search));

    FreeAndNil(abcolls);
  end;


{ cBookRepository }

function cBookRepository.getAuthors(model: T): specialize cORMModelCollection<cAuthorModel>;
  begin
  end;


{ cAuthorBookRepository }

function cAuthorBookRepository.getAuthorsFromBook(model: cBookModel): specialize cORMModelCollection<cAuthorModel>;
  begin
  end;

function cAuthorBookRepository.getBooksFromAuthor(model: cAuthorModel): specialize cORMModelCollection<cBookModel>;
  begin
  end;

function cAuthorBookRepository.linkAuthorBook(authormodel: cAuthorModel; bookmodel: cBookModel): Boolean;
  var
     bmodels: TArrBook;

  begin
    SetLength(bmodels, 1);
    bmodels[0] := bookmodel;
    result := linkAuthorBook(authormodel, bmodels);
  end;

function cAuthorBookRepository.linkAuthorBook(authormodel: cAuthorModel; bookmodels: specialize TArray<cBookModel>): Boolean;
  var
     amodels: TArrAuthor;

  begin
    SetLength(amodels, 1);
    amodels[0] := authormodel;
    result := linkAuthorBook(amodels, bookmodels);
  end;

function cAuthorBookRepository.linkAuthorBook(authormodels: specialize TArray<cAuthorModel>; bookmodel: cBookModel): Boolean;
  var
     bmodels: TArrBook;

  begin
    SetLength(bmodels, 1);
    bmodels[0] := bookmodel;
    result := linkAuthorBook(authormodels, bmodels);
  end;

function cAuthorBookRepository.linkAuthorBook(authormodels: specialize TArray<cAuthorModel>; bookmodels: specialize TArray<cBookModel>): Boolean;
  var
     mauthorbook: cAuthorBookModel;
     I,J: Integer;
     success: Boolean;

  begin
    success := true;

    for I := 0 to Length(authormodels)-1 do
      begin
        for J := 0 to Length(bookmodels)-1 do
          begin
            mauthorbook := cAuthorBookModel.Create;
            mauthorbook.AuthorId := authormodels[I].Id;
            mauthorbook.BookId := bookmodels[J].Id;

            success := success AND insert(mauthorbook);
          end;
      end;

    result := success;
  end;

function cAuthorBookRepository.removeLinkAuthorBook(authormodel: cAuthorModel; bookmodel: cBookModel): Boolean;
  begin
  end;

end.