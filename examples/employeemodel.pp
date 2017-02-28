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

unit EmployeeModel;
{$modeswitch UnicodeStrings}
interface

uses jzorm.basemodel, jzorm.datatypes;

type
  cEmployeeModel = class(cORMBaseModel)
    private
      const
        Field_Id      = 'id';
        Field_Name    = 'name';
        Field_Salary  = 'salary';
        Field_Dept    = 'department';
        Field_Manager = 'managerid';

      var
        fId: Integer;
        fName: String;
        fSalary: Currency;
        fDept: String;
        fManagerId: Integer;

    protected
      procedure setId(setvar: Integer);
      procedure setName(setvar: String);
      procedure setSalary(setvar: Currency);
      procedure setDepartment(setvar: String);
      procedure setManagerId(setvar: Integer);

      function getId(): Integer;
      function getName(): String;
      function getSalary(): Currency;
      function getDepartment(): String;
      function getManagerId(): Integer;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property Name: String read getName write setName;
      property Salary: Currency read getSalary write setSalary;
      property Department: String read getDepartment write setDepartment;
      property ManagerId: Integer read getManagerId write setManagerId;
  end;

implementation

procedure cEmployeeModel.initField();
  begin
    addField(Field_Id, mdtInteger, true);
    addField(Field_Name, mdtString);
    addField(Field_Salary, mdtCurrency);
    addField(Field_Dept, mdtString);
    addField(Field_Manager, mdtInteger);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_Name, @setName, @getName);
    setFieldCallback(Field_Salary, @setSalary, @getSalary);
    setFieldCallback(Field_Dept, @setDepartment, @getDepartment);
    setFieldCallback(Field_Manager, @setManagerId, @getManagerId);
  end;

procedure cEmployeeModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cEmployeeModel.setName(setvar: String);
  begin
    fName := setvar;
    fieldUpdated(Field_Name);
  end;

procedure cEmployeeModel.setSalary(setvar: Currency);
  begin
    fSalary := setvar;
    fieldUpdated(Field_Salary);
  end;

procedure cEmployeeModel.setDepartment(setvar: String);
  begin
    fDept := setvar;
    fieldUpdated(Field_Dept);
  end;

procedure cEmployeeModel.setManagerId(setvar: Integer);
  begin
    fManagerId := setvar;
    fieldUpdated(Field_Manager);
  end;

function cEmployeeModel.getId(): Integer;
  begin
    result := fId;
  end;

function cEmployeeModel.getName(): String;
  begin
    result := fName;
  end;

function cEmployeeModel.getSalary(): Currency;
  begin
    result := fSalary;
  end;

function cEmployeeModel.getDepartment(): String;
  begin
    result := fDept;
  end;

function cEmployeeModel.getManagerId(): Integer;
  begin
    result := fManagerId;
  end;

end.