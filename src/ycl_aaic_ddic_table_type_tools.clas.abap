CLASS ycl_aaic_ddic_table_type_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_table_type_name   TYPE yde_aaic_fc_table_type_name
                i_description       TYPE yde_aaic_fc_description
                i_row_type          TYPE yde_aaic_fc_data_type OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_ddic_table_type_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_error) = abap_false.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(l_table_type_name) = CONV sxco_ad_object_name( condense( to_upper( i_table_type_name ) ) ).

    DATA(l_row_type) = CONV sxco_ad_object_name( condense( to_upper( i_row_type ) ) ).

    DATA(lo_specification) = lo_put_operation->for-ttyp->add_object( l_table_type_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description(  i_description ).

    lo_specification->set_row_type( xco_cp_abap_dictionary=>structure( l_row_type ) ).

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error: { lo_cx_xco_gen_put_exception->get_text( ) }|.

        RETURN.

    ENDTRY.

    DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

    IF l_contain_errors = abap_false.

      r_response = |Table Type `{ l_table_type_name }` created successfully!|.

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
        i_table_type_name   = 'ZTT_CJS_STRUC_XCO_TEST'
        i_description       = 'XCO Test'
        i_row_type          = 'ZST_CJS_STRUC_XCO_TEST'
        i_transport_request = 'TRLK900008'
        i_package           = 'ZCHRJS'
    ).

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
