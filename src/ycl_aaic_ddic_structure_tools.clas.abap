CLASS ycl_aaic_ddic_structure_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_structure_name    TYPE yde_aaic_fc_structure_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
                i_t_components      TYPE ytt_aaic_fc_struct_fields
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_ddic_structure_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_error) = abap_false.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(l_structure_name) = CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ).

    DATA(lo_specification) = lo_put_operation->for-tabl-for-structure->add_object( l_structure_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( CONV #( i_description ) ).

    LOOP AT i_t_components INTO DATA(ls_component).

      ls_component-field_name = condense( to_upper( ls_component-field_name ) ).

      IF ls_component-data_element IS NOT INITIAL.

        ls_component-data_element = condense( to_upper( ls_component-data_element ) ).

        lo_specification->add_component( ls_component-field_name )->set_type( xco_cp_abap_dictionary=>data_element( ls_component-data_element ) ).

        CONTINUE.

      ENDIF.

      IF ls_component-data_type IS NOT INITIAL.

        ls_component-data_type = condense( to_upper( ls_component-data_type ) ).

        NEW ycl_aaic_ddic_tools_util( )->determine_format(
          EXPORTING
            i_data_type = CONV #( ls_component-data_type )
            i_length    = ls_component-length
            i_decimals  = ls_component-decimals
          IMPORTING
            e_o_format  = lo_format
            e_error     = DATA(l_error_description)
        ).

        IF lo_format IS BOUND.

          lo_specification->add_component( ls_component-field_name )->set_type( lo_format ).

          CONTINUE.

        ELSE.
          r_response = l_error_description.
        ENDIF.

      ENDIF.

      l_error = abap_true.

      EXIT.

    ENDLOOP.

    IF l_error = abap_true.

      IF r_response IS INITIAL.
        r_response = 'For each structure field, you must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
        r_response = r_response && 'The ABAP built-in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
      ENDIF.

      RETURN.

    ENDIF.

    DATA(lo_result) = lo_put_operation->execute( ).

    DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

    IF l_contain_errors = abap_false.

      r_response = |Structure `{ l_structure_name }` created successfully!|.

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
        i_structure_name    = 'ZST_CJS_STRUC_XCO_TEST'
        i_description       = 'XCO Test'
        i_transport_request = 'TRLK900008'
        i_package           = 'ZCHRJS'
        i_t_components      = VALUE #( ( field_name = 'FIELD1' data_element = 'ZDS_CJS_EMAIL_FROM' )
                                       ( field_name = 'FIELD2' data_type = 'CHAR' length = '30' ) )
    ).

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
