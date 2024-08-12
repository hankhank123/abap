@EndUserText.label: 'Entities of RAP Behavior'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_RAP_ENTITY_VH_QUERY'
@UI.headerInfo : { typeName: 'Entity', typeNamePlural: 'Entities'  }
@ObjectModel.resultSet.sizeCategory: #XS
define custom entity zvtest_vh2
{
   @UI.hidden: true
  key rootName  : abp_root_entity_name;
      @ObjectModel.filter.enabled: false
      @Consumption.filter.hidden: false
  key name      : abp_entity_name;
      @ObjectModel.filter.enabled: false
      @Consumption.filter.hidden: false
      aliasName : abap.char(30);
      action: abap.char(30);
      trdata: abap.string;
      testname : abap.string;
  
}
