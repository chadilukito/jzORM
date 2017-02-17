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

unit CustomerModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cCustomerModel = class(cORMBaseModel)
    private
      const
        Field_Id      = 'id';
        Field_Name    = 'name';
        Field_Address = 'address';
        Field_Age     = 'age';
        Field_Info    = 'info';

      var
        fId: Integer;
        fName: String;
        fAddress: String;
        fAge: Integer;
        fInfo: String;

    protected
      procedure setId(setvar: Integer);
      procedure setName(setvar: String);
      procedure setAddress(setvar: String);
      procedure setAge(setvar: Integer);
      procedure setInfo(setvar: String);

      function getId(): Integer;
      function getName(): String;
      function getAddress(): String;
      function getAge(): Integer;
      function getInfo(): String;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property Name: String read getName write setName;
      property Address: String read getAddress write setAddress;
      property Age: Integer read getAge write setAge;
      property Info: String read getInfo write setInfo;
  end;

implementation

procedure cCustomerModel.initField();
  begin
    addField(Field_Id, mdtInteger, true);
    addField(Field_Name, mdtString);
    addField(Field_Address, mdtString);
    addField(Field_Age, mdtInteger);
    addField(Field_Info, mdtString);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Name, @setName, @getName);
    setFieldCallback(Field_Address, @setAddress, @getAddress);
    setFieldCallback(Field_Age, @setAge, @getAge);
    setFieldCallback(Field_Info, @setInfo, @getInfo);
  end;

procedure cCustomerModel.setId(setvar: Integer);
  begin
    fieldUpdated(Field_Id);
    fId := setvar;
  end;

procedure cCustomerModel.setName(setvar: String);
  begin
    fieldUpdated(Field_Name);
    fName := setvar;
  end;

procedure cCustomerModel.setAddress(setvar: String);
  begin
    fieldUpdated(Field_Address);
    fAddress := setvar;
  end;

procedure cCustomerModel.setAge(setvar: Integer);
  begin
    fieldUpdated(Field_Age);
    fAge := setvar;
  end;

procedure cCustomerModel.setInfo(setvar: String);
  begin
    fieldUpdated(Field_Info);
    fInfo := setvar;
  end;

function cCustomerModel.getId(): Integer;
  begin
    result := fId;
  end;

function cCustomerModel.getName(): String;
  begin
    result := fName;
  end;

function cCustomerModel.getAddress(): String;
  begin
    result := fAddress;
  end;

function cCustomerModel.getAge(): Integer;
  begin
    result := fAge;
  end;

function cCustomerModel.getInfo(): String;
  begin
    result := fInfo;
  end;

end.