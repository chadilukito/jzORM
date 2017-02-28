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

unit MasterModel;
{$modeswitch UnicodeStrings}
{$H+}
interface

uses jzorm.basemodel, jzorm.datatypes, employeemodel, detailsmodel, datetimeclass;

type
  cMasterModel = class(cORMBaseModel)
    private
      const
        Field_Id        = 'id';
        Field_TransDate = 'transaction_date';
        Field_Total     = 'total';
        Field_Employee  = 'employeeid';

      var
        fId: Integer;
        fTransDate: cDatetime;
        fTotal: Currency;
        fEmployeeId: Integer;

    protected
      procedure setId(setvar: Integer);
      procedure setTransDate(setvar: cDatetime);
      procedure setTotal(setvar: Currency);
      procedure setEmployeeId(setvar: Integer);

      function getId(): Integer;
      function getTransDate(): cDatetime;
      function getTotal(): Currency;
      function getEmployeeId(): Integer;
      function getEmployee(): cEmployeeModel;

      procedure initField(); override;

    public
      property ID: Integer read getId write setId;
      property TransactionDatetime: cDatetime read getTransDate write setTransDate;
      property Total: Currency read getTotal write setTotal;
      property EmployeeId: Integer read getEmployeeId write setEmployeeId;
      property Employee: cEmployeeModel read getEmployee;
  end;

implementation

procedure cMasterModel.initField();
  begin
    addField(Field_Id, mdtInteger, true);
    addField(Field_TransDate, mdtDatetime);
    addField(Field_Total, mdtCurrency);
    addField(Field_Employee, mdtInteger);

    setFieldCallback(Field_Id, @setId, @getId);
    setFieldCallback(Field_TransDate, @setTransDate, @getTransDate);
    setFieldCallback(Field_Total, @setTotal, @getTotal);
    setFieldCallback(Field_Employee, @setEmployeeId, @getEmployeeId);
  end;

procedure cMasterModel.setId(setvar: Integer);
  begin
    fId := setvar;
    fieldUpdated(Field_Id);
  end;

procedure cMasterModel.setTransDate(setvar: cDatetime);
  begin
    fTransDate := setvar;
    fieldUpdated(Field_TransDate);
  end;

procedure cMasterModel.setTotal(setvar: Currency);
  begin
    fTotal := setvar;
    fieldUpdated(Field_Total);
  end;

procedure cMasterModel.setEmployeeId(setvar: Integer);
  begin
    fEmployeeId := setvar;
    fieldUpdated(Field_Employee);
  end;

function cMasterModel.getId(): Integer;
  begin
    result := fId;
  end;

function cMasterModel.getTransDate(): cDatetime;
  begin
    result := fTransDate;
  end;

function cMasterModel.getTotal(): Currency;
  begin
    result := fTotal;
  end;

function cMasterModel.getEmployeeId(): Integer;
  begin
    result := fEmployeeId;
  end;

function cMasterModel.getEmployee(): cEmployeeModel;
  begin
  end;

end.