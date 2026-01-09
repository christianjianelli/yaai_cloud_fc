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

    "Transport request tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_transp_request_tools' method_name = 'create' description = 'Use this tool to create a transport request in the ABAP system.' )
                                             ( class_name = 'ycl_aaic_transp_request_tools' method_name = 'search' description = 'Use this tool to search for transport requests in the ABAP system.' ) ) )
      ACCEPTING DUPLICATE KEYS.

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
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'search' description = 'Use this tool to search for ABAP data element in the ABAP Dictionary.' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'set_translation' description = 'Use this tool to create a translation for the data element labels (short texts).' )
                                             ( class_name = 'ycl_aaic_data_element_tools' method_name = 'get_translation' description = 'Use this tool to get the translation of the data element labels (short texts).' ) ) )
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

    "Message Class tools
    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_message_class_tools' method_name = 'create_message' description = 'Use this tool to create a message in a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'read_message'   description = 'Use this tool to read a message of a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'update_message' description = 'Use this tool to update a message in a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'delete_message' description = 'Use this tool to delete a message in a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'read_all_messages' description = 'Use this tool to get all messages of a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'set_translation' description = 'Use this tool to create a translation for a message in a message class.' )
                                             ( class_name = 'ycl_aaic_message_class_tools' method_name = 'get_translation' description = 'Use this tool to get the translation of a single message or all messages of a message class.' ) ) )
      ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
