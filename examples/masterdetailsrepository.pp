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

unit MasterDetailsRepository;
{$modeswitch UnicodeStrings}
interface

uses jzorm.baserepository, mastermodel, detailsmodel, jzorm.modelcollection, employeemodel;

type
  generic cMasterRepository<T: cMasterModel> = class(specialize cORMRepository<T>)
    private
      type
        TArrMDets = specialize cORMModelCollection<cDetailsModel>;

    public
      function getDetails(model: T): specialize cORMModelCollection<cDetailsModel>;
      function getEmployee(model: T): cEmployeeModel;
      procedure updateTotal(model: T);
  end;

  generic cDetailsRepository<T: cDetailsModel> = class(specialize cORMRepository<T>)
    public
      function getMaster(model: T): cMasterModel;
  end;

  cMasterDetailsRepository = class
    public
  end;

implementation

uses sysutils;

{ cMasterRepository }

function cMasterRepository.getDetails(model: T): specialize cORMModelCollection<cDetailsModel>;
  var
     repod: specialize cDetailsRepository<cDetailsModel>;

  begin
    repod := specialize cDetailsRepository<cDetailsModel>.Create(ormConnect, 'details');
    result := repod.findBy(cSearchCriteria.Create(1, cSearchField.Create('masterid', TSearchFieldOperator.sfoEqual, model.Id)));
  end;

function cMasterRepository.getEmployee(model: T): cEmployeeModel;
  begin
    //result := findOneBy(cSearchCriteria.Create(1, cSearchField.Create('id', TSearchFieldOperator.sfoEqual, model.ManagerId)));
  end;

procedure cMasterRepository.updateTotal(model: T);
  var
     mdets: cDetailsModel;
     total: Currency;
     arrmdets: TArrMDets;

  begin
    total := 0;
    arrmdets := getDetails(model);
    for mdets in arrmdets do
      begin
        total := total+mdets.total;
      end;

    model.Total := total;

    FreeAndNil(arrmdets);
  end;


{ cDetailsRepository }

function cDetailsRepository.getMaster(model: T): cMasterModel;
  begin
    {if (model.Fields[model.searchFieldByName('managerid')].valueSet) then
      begin
        result := findOneBy(cSearchCriteria.Create(1, cSearchField.Create('managerid', TSearchFieldOperator.sfoEqual, model.Id)));
      end else
      result := nil;}
  end;

end.