CLASS ycl_aaic_domain_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_domain_name       TYPE string
                i_description       TYPE string
                i_data_type         TYPE string
                i_length            TYPE i OPTIONAL
                i_decimals          TYPE i OPTIONAL
                i_case_sensitive    TYPE abap_boolean DEFAULT abap_false
                i_transport_request TYPE string
                i_package           TYPE string
                i_t_fixed_values    TYPE ytt_aaic_fc_domain_fixed_val OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS determine_format
      IMPORTING
        i_data_type TYPE string
        i_length    TYPE i OPTIONAL
        i_decimals  TYPE i OPTIONAL
      EXPORTING
        e_o_format  TYPE REF TO cl_xco_ad_built_in_type
        e_error     TYPE string.

ENDCLASS.



CLASS ycl_aaic_domain_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-doma->add_object( l_domain_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( CONV #( i_description ) ).

    me->determine_format(
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
      r_response = 'The ABAP built in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
      r_response = |The data type { i_data_type } is incorrect or invalid. Only ABAP built in types are allowed. { r_response }|.
      RETURN.
    ENDIF.

    lo_specification->set_format( lo_format ).

    lo_specification->output_characteristics->set_case_sensitive( i_case_sensitive ).


    LOOP AT i_t_fixed_values INTO DATA(ls_fixed_value).

      ls_fixed_value-value = condense( to_upper( ls_fixed_value-value ) ).

      lo_specification->fixed_values->add_fixed_value( ls_fixed_value-value
        )->set_description( ls_fixed_value-description ).

    ENDLOOP.

    DATA(lo_result) = lo_put_operation->execute( ).

    DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

    IF l_contain_errors = abap_false.

      r_response = |Domain `{ l_domain_name }` created successfully!|.

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

  METHOD determine_format.

    CLEAR e_o_format.

    DATA(l_data_type) = condense( to_upper( i_data_type ) ).

    CASE l_data_type.

      WHEN 'CHAR'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->char( CONV #( i_length ) ).

      WHEN 'INT1'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int1.

      WHEN 'INT2'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int2.

      WHEN 'INT4'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int4.

      WHEN 'DEC'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->dec( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'NUMC'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->numc( CONV #( i_length ) ).

      WHEN 'QUAN'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->quan( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'STRING'.

        IF i_length > 0 AND i_length < 256.
          e_o_format = xco_cp_abap_dictionary=>built_in_type->string( CONV #( 256 ) ).
        ELSE.
          e_o_format = xco_cp_abap_dictionary=>built_in_type->string( CONV #( i_length ) ).
        ENDIF.

      WHEN 'UNIT'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->unit( CONV #( 2 ) ).

      WHEN 'CURR'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->curr( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'CUKY'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->cuky.

      WHEN 'DATS'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->dats.

      WHEN 'TIMS'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->tims.

      WHEN 'FLTP'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->fltp.

      WHEN 'LANG'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->lang.

      WHEN 'CLNT'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->clnt.

      WHEN OTHERS.

        RETURN.

    ENDCASE.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA(l_response) = me->create(
      EXPORTING
        i_domain_name        = 'ZDO_CJS_XCO_TEST_QUAN'
        i_description        = 'XCO Domain generation test'
        i_data_type          = 'QUAN'
        i_length             = '13'
        i_decimals           = '3'
        i_case_sensitive     = abap_true
        i_transport_request  = 'TRLK900008'
        i_package            = 'ZCHRJS'
*        i_t_fixed_values     = VALUE #( ( value = 'A' description = 'VALUE A' )
*                                        ( value = 'B' description = 'VALUE B' ) )
    ).

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
