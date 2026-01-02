CLASS ycl_aaic_fc_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_fc_setup IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "Domain tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_domain_tools' method_name = 'create' description = 'Use this tool to create an ABAP domain in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_domain_tools' method_name = 'read'   description = 'Use this tool to read an ABAP domain in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_domain_tools' method_name = 'update' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_domain_tools' method_name = 'delete' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_domain_tools' method_name = 'search' description = 'Use this tool to search for ABAP domains in the ABAP Dictionary.' ) ) )
      ACCEPTING DUPLICATE KEYS.

    "Data Element tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_data_element_tools' method_name = 'create' description = 'Use this tool to create an ABAP data element in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'read'   description = 'Use this tool to read an ABAP data element in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'update' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'delete' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'search' description = 'Use this tool to search for ABAP data element in the ABAP Dictionary.' ) ) )
      ACCEPTING DUPLICATE KEYS.

    "DDIC Structure tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'create' description = 'Use this tool to create an ABAP structure in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'read'   description = 'Use this tool to read an ABAP structure in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'update' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'delete' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'search' description = 'Use this tool to search for ABAP structures in the ABAP Dictionary.' ) ) )
      ACCEPTING DUPLICATE KEYS.

    "DDIC Table tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'create' description = 'Use this tool to create an ABAP table in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'read'   description = 'Use this tool to read an ABAP table in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'update' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'delete' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'search' description = 'Use this tool to search for ABAP tables in the ABAP Dictionary.' ) ) )
      ACCEPTING DUPLICATE KEYS.

    "DDIC Table Type tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'create' description = 'Use this tool to create an ABAP table type in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'read'   description = 'Use this tool to read an ABAP table type in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'update' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'delete' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'search' description = 'Use this tool to search for ABAP table types in the ABAP Dictionary.' ) ) )
      ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
