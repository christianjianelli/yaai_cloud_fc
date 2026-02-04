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
        ( class_name = 'YCL_AAIC_TRANSP_REQUEST_TOOLS' method_name = 'CREATE' description = 'Use this tool to create a transport request in the ABAP system.' )
        ( class_name = 'YCL_AAIC_TRANSP_REQUEST_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for transport requests in the ABAP system.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

    "Domain tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_DOMAIN_TOOLS' method_name = 'CREATE' description = 'Use this tool to create an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DOMAIN_TOOLS' method_name = 'READ'   description = 'Use this tool to read an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DOMAIN_TOOLS' method_name = 'UPDATE' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DOMAIN_TOOLS' method_name = 'DELETE' description = 'Use this tool to update an ABAP domain in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DOMAIN_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for ABAP domains in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "Data Element tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'CREATE' description = 'Use this tool to create an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'READ'   description = 'Use this tool to read an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'UPDATE' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'DELETE' description = 'Use this tool to update an ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for ABAP data element in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'SET_TRANSLATION' description = 'Use this tool to create a translation for the data element labels (short texts).' )
        ( class_name = 'YCL_AAIC_DATA_ELEMENT_TOOLS' method_name = 'GET_TRANSLATION' description = 'Use this tool to get the translation of the data element labels (short texts).' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Structure tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_DDIC_STRUCTURE_TOOLS' method_name = 'CREATE' description = 'Use this tool to create an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_STRUCTURE_TOOLS' method_name = 'READ'   description = 'Use this tool to read an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_STRUCTURE_TOOLS' method_name = 'UPDATE' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_STRUCTURE_TOOLS' method_name = 'DELETE' description = 'Use this tool to update an ABAP structure in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_STRUCTURE_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for ABAP structures in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Table tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TOOLS' method_name = 'CREATE' description = 'Use this tool to create an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TOOLS' method_name = 'READ'   description = 'Use this tool to read an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TOOLS' method_name = 'UPDATE' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TOOLS' method_name = 'DELETE' description = 'Use this tool to update an ABAP table in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for ABAP tables in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "DDIC Table Type tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TYPE_TOOLS' method_name = 'CREATE' description = 'Use this tool to create an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TYPE_TOOLS' method_name = 'READ'   description = 'Use this tool to read an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TYPE_TOOLS' method_name = 'UPDATE' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TYPE_TOOLS' method_name = 'DELETE' description = 'Use this tool to update an ABAP table type in the ABAP Dictionary.' )
        ( class_name = 'YCL_AAIC_DDIC_TABLE_TYPE_TOOLS' method_name = 'SEARCH' description = 'Use this tool to search for ABAP table types in the ABAP Dictionary.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "Message Class tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'CREATE_MESSAGE' description = 'Use this tool to create a message in a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'READ_MESSAGE'   description = 'Use this tool to read a message of a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'UPDATE_MESSAGE' description = 'Use this tool to update a message in a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'DELETE_MESSAGE' description = 'Use this tool to delete a message in a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'READ_ALL_MESSAGES' description = 'Use this tool to get all messages of a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'SET_TRANSLATION' description = 'Use this tool to create a translation for a message in a message class.' )
        ( class_name = 'YCL_AAIC_MESSAGE_CLASS_TOOLS' method_name = 'GET_TRANSLATION' description = 'Use this tool to get the translation of a single message or all messages of a message class.' )
     ) )
       ACCEPTING DUPLICATE KEYS.

    "ABAP Class tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'CREATE' description = 'Use this tool to create a new ABAP class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'ADD_METHOD' description = 'Use this tool to add a method to an existing ABAP class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'ADD_METHOD_PARAMETERS' description = 'Use this tool to add parameters to an existing method of a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'CHANGE_METHOD_IMPLEMENTATION' description = 'Use this tool to change the implementation (source code) of a method.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'DELETE_METHOD' description = 'Use this tool to delete a method from a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'ADD_ATTRIBUTE' description = 'Use this tool to add an attribute to a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'ADD_CONSTANT' description = 'Use this tool to add a constant to a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'DELETE_ATTRIBUTE' description = 'Use this tool to delete an attribute from a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'DELETE_CONSTANT' description = 'Use this tool to delete a constant from a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'GET_CLASS_DEFINITION' description = 'Use this tool to retrieve the full definition of an ABAP class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'GET_METHOD_DEFINITION' description = 'Use this tool to retrieve the signature/definition of a method.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'GET_METHOD_IMPLEMENTATION' description = 'Use this tool to retrieve the implementation (source code) of a method.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'GET_TRANSLATION' description = 'Use this tool to read a text symbol translation from a class.' )
        ( class_name = 'YCL_AAIC_CLASS_TOOLS' method_name = 'SET_TRANSLATION' description = 'Use this tool to set a text symbol translation for a class.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

    "ABAP Interface tools
    INSERT yaaic_tool FROM TABLE @( VALUE #(
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'CREATE' description = 'Use this tool to create a new ABAP interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'ADD_METHOD' description = 'Use this tool to add a method to an existing ABAP interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'ADD_METHOD_PARAMETERS' description = 'Use this tool to add parameters to an existing method of an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'DELETE_METHOD_PARAMETERS' description = 'Use this tool to delete parameters from an existing method in an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'DELETE_METHOD' description = 'Use this tool to delete a method from an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'ADD_ATTRIBUTE' description = 'Use this tool to add an attribute to an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'ADD_CONSTANT' description = 'Use this tool to add a constant to an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'DELETE_ATTRIBUTE' description = 'Use this tool to delete an attribute from an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'DELETE_CONSTANT' description = 'Use this tool to delete a constant from an interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'GET_INTERFACE_DEFINITION' description = 'Use this tool to retrieve the full definition of an ABAP interface.' )
        ( class_name = 'YCL_AAIC_INTERFACE_TOOLS' method_name = 'GET_METHOD_DEFINITION' description = 'Use this tool to retrieve the signature/definition of a method in an interface.' )
    ) )
      ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.

ENDCLASS.
