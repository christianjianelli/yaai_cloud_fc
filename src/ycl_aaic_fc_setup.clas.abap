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
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_transp_request_tools' method_name = 'create' description = 'Use this tool to create a transport request in the ABAP system.' )
        ( class_name = 'ycl_aaic_transp_request_tools' method_name = 'search' description = 'Use this tool to search for transport requests in the ABAP system.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

    "Domain tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_domain_tools' method_name = 'create' description = 'Use this tool to create an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_domain_tools' method_name = 'read'   description = 'Use this tool to read an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_domain_tools' method_name = 'update' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_domain_tools' method_name = 'delete' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_domain_tools' method_name = 'search' description = 'Use this tool to search for ABAP domains in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "Data Element tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'create' description = 'Use this tool to create an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'read'   description = 'Use this tool to read an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'update' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'delete' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'search' description = 'Use this tool to search for ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'set_translation' description = 'Use this tool to create a translation for the data element labels (short texts).' )
        ( class_name = 'ycl_aaic_data_element_tools' method_name = 'get_translation' description = 'Use this tool to get the translation of the data element labels (short texts).' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Structure tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'create' description = 'Use this tool to create an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'read'   description = 'Use this tool to read an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'update' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'delete' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_structure_tools' method_name = 'search' description = 'Use this tool to search for ABAP structures in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Table tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'create' description = 'Use this tool to create an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'read'   description = 'Use this tool to read an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'update' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'delete' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_tools' method_name = 'search' description = 'Use this tool to search for ABAP tables in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Table Type tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'create' description = 'Use this tool to create an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'read'   description = 'Use this tool to read an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'update' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'delete' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'ycl_aaic_ddic_table_type_tools' method_name = 'search' description = 'Use this tool to search for ABAP table types in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "Message Class tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'create_message' description = 'Use this tool to create a message in a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'read_message'   description = 'Use this tool to read a message of a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'update_message' description = 'Use this tool to update a message in a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'delete_message' description = 'Use this tool to delete a message in a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'read_all_messages' description = 'Use this tool to get all messages of a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'set_translation' description = 'Use this tool to create a translation for a message in a message class.' )
        ( class_name = 'ycl_aaic_message_class_tools' method_name = 'get_translation' description = 'Use this tool to get the translation of a single message or all messages of a message class.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "ABAP Class tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_class_tools' method_name = 'create' description = 'Use this tool to create a new ABAP class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'add_method' description = 'Use this tool to add a method to an existing ABAP class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'add_method_parameters' description = 'Use this tool to add parameters to an existing method of a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'change_method_implementation' description = 'Use this tool to change the implementation (source code) of a method.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'delete_method' description = 'Use this tool to delete a method from a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'add_attribute' description = 'Use this tool to add an attribute to a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'add_constant' description = 'Use this tool to add a constant to a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'delete_attribute' description = 'Use this tool to delete an attribute from a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'delete_constant' description = 'Use this tool to delete a constant from a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'get_class_definition' description = 'Use this tool to retrieve the full definition of an ABAP class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'get_method_definition' description = 'Use this tool to retrieve the signature/definition of a method.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'get_method_implementation' description = 'Use this tool to retrieve the implementation (source code) of a method.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'get_translation' description = 'Use this tool to read a text symbol translation from a class.' )
        ( class_name = 'ycl_aaic_class_tools' method_name = 'set_translation' description = 'Use this tool to set a text symbol translation for a class.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

    "ABAP Interface tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'create' description = 'Use this tool to create a new ABAP interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'add_method' description = 'Use this tool to add a method to an existing ABAP interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'add_method_parameters' description = 'Use this tool to add parameters to an existing method of an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'delete_method_parameters' description = 'Use this tool to delete parameters from an existing method in an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'delete_method' description = 'Use this tool to delete a method from an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'add_attribute' description = 'Use this tool to add an attribute to an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'add_constant' description = 'Use this tool to add a constant to an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'delete_attribute' description = 'Use this tool to delete an attribute from an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'delete_constant' description = 'Use this tool to delete a constant from an interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'get_interface_definition' description = 'Use this tool to retrieve the full definition of an ABAP interface.' )
        ( class_name = 'ycl_aaic_interface_tools' method_name = 'get_method_definition' description = 'Use this tool to retrieve the signature/definition of a method in an interface.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
