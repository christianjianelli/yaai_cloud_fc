CLASS ycl_aaic_data_element_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_data_element_name TYPE string
                i_description       TYPE string
                i_domain_name       TYPE string OPTIONAL
                i_data_type         TYPE string OPTIONAL
                i_length            TYPE i OPTIONAL
                i_decimals          TYPE i OPTIONAL
                i_transport_request TYPE string
                i_package           TYPE string
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_data_element_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    DATA l_domain_name TYPE sxco_ad_object_name.

    CLEAR r_response.

    IF i_domain_name IS INITIAL AND i_data_type IS INITIAL.
      r_response = 'You must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
      r_response = 'The ABAP built-in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_data_element_name) = CONV sxco_ad_object_name( condense( to_upper( i_data_element_name ) ) ).

    IF i_domain_name IS NOT INITIAL.
      l_domain_name = condense( to_upper( i_domain_name ) ).
    ELSE.

      NEW ycl_aaic_ddic_tools_util( )->determine_format(
        EXPORTING
          i_data_type = i_data_type
          i_length    = i_length
          i_decimals  = i_decimals
        IMPORTING
          e_o_format  = lo_format
          e_error     = DATA(l_error)
      ).

      IF l_error IS NOT INITIAL.
        r_response = l_error.
        RETURN.
      ENDIF.

      IF lo_format IS NOT BOUND.
        r_response = 'The ABAP built-in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
        r_response = |The data type { i_data_type } is incorrect or invalid. Only ABAP built-in types are allowed. { r_response }|.
        RETURN.
      ENDIF.

    ENDIF.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-dtel->add_object( l_data_element_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( CONV #( i_description ) ).

    IF l_domain_name IS NOT INITIAL.

      lo_specification->set_data_type( xco_cp_abap_dictionary=>domain( l_domain_name ) ).

    ELSEIF lo_format IS BOUND.

      lo_specification->set_data_type( lo_format ).

    ENDIF.

    DATA(lo_result) = lo_put_operation->execute( ).

    DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

    IF l_contain_errors = abap_false.

      r_response = |Data Element `{ l_data_element_name }` created successfully!|.

    ELSE.

      DATA(lt_findings) = lo_result->findings->get( ).

      LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

        IF r_response IS NOT INITIAL.
          r_response = r_response && cl_abap_char_utilities=>newline.
        ENDIF.

        r_response = r_response && <ls_finding>->message->get_text( ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA(l_response) = me->create(
      EXPORTING
        i_data_element_name  = 'ZDS_CJS_EMAIL_FROM'
        i_description        = 'XCO Domain generation test'
*        i_domain_name        = 'ZDO_CJS_EMAIL_FROM'
        i_data_type          = 'CHAR'
        i_length             = '100'
*        i_decimals           = '3'
        i_transport_request  = 'TRLK900008'
        i_package            = 'ZCHRJS'
    ).

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
