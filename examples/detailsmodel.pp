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

unit DetailsModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cDetailsModel = class(cORMBaseModel)
    private
      const
        Field_Id     = 'id';
        Field_Master = 'masterid';
        Field_Name   = 'name';
        Field_Price  = 'price';
        Field_Count  = 'count';
        Field_Total  = 'total';

      var
        fId: Integer;
        fMaster: Integer;
        fName: String;
        fPrice: Currency;
        fCount: Integer;
        fTotal: Currency;

    protected
      procedure setId(setvar: Integer);
      procedure setMasterId(setvar: Integer);
      procedure setName(setvar: String);
      procedure setPrice(setvar: Currency);
      procedure setCount(setvar: Integer);
      procedure setTotal(setvar: Currency);

      function getId(): Integer;
      function getMasterId(): Integer;
      function getName(): String;
      function getPrice(): Currency;
      function getCount(): Integer;
      function getTotal(): Currency;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property MasterId: Integer read getMasterId write setMasterId;
      property Name: String read getName write setName;
      property Price: Currency read getPrice write setPrice;
      property Count: Integer read getCount write setCount;
      property Total: Currency read getTotal;// write setTotal;
  end;

implementation

procedure cDetailsModel.initField();
  begin
    addField(Field_Id, mdtInteger, true);
    addField(Field_Master, mdtInteger);
    addField(Field_Name, mdtString);
    addField(Field_Price, mdtCurrency);
    addField(Field_Count, mdtInteger);
    addField(Field_Total, mdtCurrency);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Master, @setMasterId, @getMasterId);
    setFieldCallback(Field_Name, @setName, @getName);
    setFieldCallback(Field_Price, @setPrice, @getPrice);
    setFieldCallback(Field_Count, @setCount, @getCount);
    setFieldCallback(Field_Total, @setTotal, @getTotal);
  end;

procedure cDetailsModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cDetailsModel.setMasterId(setvar: Integer);
  begin
    fMaster := setvar;
    fieldUpdated(Field_Master);
  end;

procedure cDetailsModel.setName(setvar: String);
  begin
    fName := setvar;
    fieldUpdated(Field_Name);
    setTotal(fPrice * fCount);
  end;

procedure cDetailsModel.setPrice(setvar: Currency);
  begin
    fPrice := setvar;
    fieldUpdated(Field_Price);
  end;

procedure cDetailsModel.setCount(setvar: Integer);
  begin
    fCount := setvar;
    fieldUpdated(Field_Count);
    setTotal(fPrice * fCount);
  end;

procedure cDetailsModel.setTotal(setvar: Currency);
  begin
    fTotal := setvar;
    fieldUpdated(Field_Total);
  end;

function cDetailsModel.getId(): Integer;
  begin
    result := fId;
  end;

function cDetailsModel.getMasterId(): Integer;
  begin
    result := fMaster;
  end;

function cDetailsModel.getName(): String;
  begin
    result := fName;
  end;

function cDetailsModel.getPrice(): Currency;
  begin
    result := fPrice;
  end;

function cDetailsModel.getCount(): Integer;
  begin
    result := fCount;
  end;

function cDetailsModel.getTotal(): Currency;
  begin
    result := fTotal;
  end;


end.