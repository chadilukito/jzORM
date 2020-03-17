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

unit EmployeeRepository;
{$modeswitch UnicodeStrings}
interface

uses jzorm.baserepository, employeemodel, jzorm.modelcollection;

type
  generic cEmployeeRepository<T: cEmployeeModel> = class(specialize cORMRepository<T>)
    private
      type
        TCollModel = specialize cORMModelCollection<T>;
      
    public
      function getManager(model: T): cEmployeeModel;
      function getSubOrdinate(model: T): specialize cORMModelCollection<T>;
      function removeManager(model: T): Boolean;
  end;

implementation

function cEmployeeRepository.getManager(model: T): cEmployeeModel;
  begin
    result := findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, model.ManagerId)));
  end;

function cEmployeeRepository.getSubOrdinate(model: T): specialize cORMModelCollection<T>;
  var
     idx: Integer;
  
  begin
    if (Not Assigned(model)) then
      begin
        result := TCollModel.Create(nil);
        exit;
      end;
  
    idx := model.searchFieldByName('managerid');
    if (model.Fields[idx].valueSet) then
      begin
        result := findBy(cSearchCriteria.Create(1, cSearchField.Create('managerid', TSearchFieldOperator.sfoEqual, model.Id)));
      end else
      result := TCollModel.Create(nil);
  end;

function cEmployeeRepository.removeManager(model: T): Boolean;
  begin
  end;

end.