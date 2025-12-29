CLASS ycl_aaic_ddic_table_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_table_name        TYPE yde_aaic_fc_table_name
                i_description       TYPE yde_aaic_fc_description
                i_customizing_table TYPE yde_aaic_fc_customizing_table OPTIONAL
                i_size_category     TYPE yde_aaic_fc_size_category OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
                i_t_fields          TYPE ytt_aaic_fc_table_fields
      RETURNING VALUE(r_response)   TYPE string.

    METHODS read
      IMPORTING
                i_table_name      TYPE yde_aaic_fc_table_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS update
      IMPORTING
                i_table_name        TYPE yde_aaic_fc_table_name
                i_description       TYPE yde_aaic_fc_description
                i_customizing_table TYPE yde_aaic_fc_customizing_table OPTIONAL
                i_size_category     TYPE yde_aaic_fc_size_category OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_t_fields          TYPE ytt_aaic_fc_table_fields
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete
      IMPORTING
                i_table_name        TYPE yde_aaic_fc_table_name
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS search
      IMPORTING
                i_table_name      TYPE yde_aaic_fc_table_name OPTIONAL
                i_description     TYPE yde_aaic_fc_description OPTIONAL
                i_package         TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response) TYPE string.

    METHODS activate
      IMPORTING
                i_table_name      TYPE yde_aaic_fc_table_name
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_ddic_table_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_error) = abap_false.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(lo_delivery_class) = COND #( WHEN i_customizing_table = abap_true THEN xco_cp_database_table=>delivery_class->c ELSE xco_cp_database_table=>delivery_class->a ).

    DATA(lo_size_category) = COND #( WHEN i_size_category IS INITIAL THEN xco_cp_database_table=>size_category->zero
                                     ELSE xco_cp_database_table=>size_category->for( CONV #( i_size_category ) ) ).

    DATA(lo_specification) = lo_put_operation->for-tabl-for-database_table->add_object( l_table_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description
      )->set_delivery_class( lo_delivery_class
      )->set_data_maintenance( xco_cp_database_table=>data_maintenance->allowed ).

    lo_specification->technical_settings-general_properties->set_size_category( lo_size_category ).

    LOOP AT i_t_fields INTO DATA(ls_field).

      ls_field-field_name = condense( to_upper( ls_field-field_name ) ).

      IF ls_field-data_element IS NOT INITIAL.

        ls_field-data_element = condense( to_upper( ls_field-data_element ) ).

        DATA(lo_field) = lo_specification->add_field( ls_field-field_name )->set_type( xco_cp_abap_dictionary=>data_element( ls_field-data_element ) ).

        IF ls_field-key_indicator = abap_true.
          lo_field->set_key_indicator(
            )->set_not_null( ).
        ENDIF.

        CONTINUE.

      ENDIF.

      IF ls_field-data_type IS NOT INITIAL.

        ls_field-data_type = condense( to_upper( ls_field-data_type ) ).

        NEW ycl_aaic_ddic_tools_util( )->determine_format(
          EXPORTING
            i_data_type = CONV #( ls_field-data_type )
            i_length    = ls_field-length
            i_decimals  = ls_field-decimals
          IMPORTING
            e_o_format  = lo_format
            e_error     = DATA(l_error_description)
        ).

        IF lo_format IS BOUND.

          lo_field = lo_specification->add_field( ls_field-field_name )->set_type( lo_format ).

          IF ls_field-key_indicator = abap_true.
            lo_field->set_key_indicator(
              )->set_not_null( ).
          ENDIF.

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
        r_response = 'For each table field, you must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
        r_response = r_response && 'The ABAP built-in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
      ENDIF.

      RETURN.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        IF lo_result->findings->contain_errors( ) = abap_false.

          r_response = |Table `{ l_table_name }` created successfully!|.

        ELSE.

          DATA(lt_findings) = lo_result->findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

            IF r_response IS NOT INITIAL.
              r_response = r_response && cl_abap_char_utilities=>newline.
            ENDIF.

            r_response = r_response && <ls_finding>->message->get_text( ).

          ENDLOOP.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error! { lo_cx_xco_gen_put_exception->get_longtext( ) }|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->tabl.

        DATA(lt_findings_ex) = lo_findings->get( ).

        LOOP AT lt_findings_ex ASSIGNING FIELD-SYMBOL(<lo_finding_ex>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <lo_finding_ex>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD read.

  ENDMETHOD.

  METHOD update.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_error) = abap_false.

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(lo_database_table) = xco_cp_abap_dictionary=>database_table( l_table_name ).

    IF lo_database_table->exists( ) = abap_false.

      r_response = |The table { l_table_name } doesn't exist.|.

      RETURN.

    ENDIF.

    DATA(l_package) = lo_database_table->if_xco_ar_object~get_package( ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_delivery_class) = COND #( WHEN i_customizing_table = abap_true THEN xco_cp_database_table=>delivery_class->c
                                      ELSE xco_cp_database_table=>delivery_class->a ).

    DATA(lo_size_category) = COND #( WHEN i_size_category IS INITIAL THEN xco_cp_database_table=>size_category->zero
                                     ELSE xco_cp_database_table=>size_category->for( CONV #( i_size_category ) ) ).

    DATA(lo_specification) = lo_put_operation->for-tabl-for-database_table->add_object( l_table_name
      )->set_package( l_package->name
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description
      )->set_delivery_class( lo_delivery_class
      )->set_data_maintenance( xco_cp_database_table=>data_maintenance->allowed ).

    lo_specification->technical_settings-general_properties->set_size_category( lo_size_category ).

    LOOP AT i_t_fields INTO DATA(ls_field).

      ls_field-field_name = condense( to_upper( ls_field-field_name ) ).

      IF ls_field-data_element IS NOT INITIAL.

        ls_field-data_element = condense( to_upper( ls_field-data_element ) ).

        DATA(lo_field) = lo_specification->add_field( ls_field-field_name )->set_type( xco_cp_abap_dictionary=>data_element( ls_field-data_element ) ).

        IF ls_field-key_indicator = abap_true.
          lo_field->set_key_indicator(
            )->set_not_null( ).
        ENDIF.

        CONTINUE.

      ENDIF.

      IF ls_field-data_type IS NOT INITIAL.

        ls_field-data_type = condense( to_upper( ls_field-data_type ) ).

        NEW ycl_aaic_ddic_tools_util( )->determine_format(
          EXPORTING
            i_data_type = CONV #( ls_field-data_type )
            i_length    = ls_field-length
            i_decimals  = ls_field-decimals
          IMPORTING
            e_o_format  = lo_format
            e_error     = DATA(l_error_description)
        ).

        IF lo_format IS BOUND.

          lo_field = lo_specification->add_field( ls_field-field_name )->set_type( lo_format ).

          IF ls_field-key_indicator = abap_true.
            lo_field->set_key_indicator(
              )->set_not_null( ).
          ENDIF.

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
        r_response = 'For each table field, you must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
        r_response = r_response && 'The ABAP built-in types supported are: CHAR, INT1, INT2, INT4, DEC, NUMC, STRING, DATS, TIMS, QUAN, UNIT, CURR, CUKY, FLTP, LANG, CLNT'.
      ENDIF.

      RETURN.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        IF lo_result->findings->contain_errors( ) = abap_false.

          r_response = |Table `{ l_table_name }` updated successfully!|.

        ELSE.

          DATA(lt_findings) = lo_result->findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

            IF r_response IS NOT INITIAL.
              r_response = r_response && cl_abap_char_utilities=>newline.
            ENDIF.

            r_response = r_response && <ls_finding>->message->get_text( ).

          ENDLOOP.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error! { lo_cx_xco_gen_put_exception->get_longtext( ) }|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->tabl.

        DATA(lt_findings_ex) = lo_findings->get( ).

        LOOP AT lt_findings_ex ASSIGNING FIELD-SYMBOL(<lo_finding_ex>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <lo_finding_ex>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD delete.

    CLEAR r_response.

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-tabl-for-database_table->create_delete_operation( ).

    lo_delete_operation->add_object( l_table_name ).

    TRY.

        DATA(lo_result) = lo_delete_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Table `{ l_table_name }` deleted successfully!|.

        ELSE.

          r_response = |Error: the Table `{ l_table_name }` was not deleted!|.

        ENDIF.

      CATCH cx_xco_gen_delete_exception INTO DATA(lo_cx_xco_gen_delete_exception).

        r_response = |Error! { lo_cx_xco_gen_delete_exception->get_longtext( ) }|.

        DATA(lo_findings) = lo_cx_xco_gen_delete_exception->findings->for->doma.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD search.

  ENDMETHOD.

  METHOD activate.

    CLEAR r_response.

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( ''
      )->create_patch_operation( ).

    DATA(lo_change_specification) = lo_patch_operation->for-tabl-for-database_table->add_object( l_table_name
      )->create_change_specification( ).

    TRY.

        DATA(lo_result) = lo_patch_operation->execute( ).

        IF lo_result->findings->contain_errors( ) = abap_false.

          r_response = |Table `{ l_table_name }` activated successfully!|.

        ENDIF.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = lo_cx_xco_gen_patch_exception->get_longtext( ).

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->doma.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_delete) = abap_true.
    DATA(l_search) = abap_false.
    DATA(l_activate) = abap_false.

    " Create Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_table_name        = 'ZTB_CJS_XCO_TEST'
          i_description       = 'XCO Test'
          i_customizing_table = abap_false
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
          i_t_fields          = VALUE #( ( field_name = 'CLIENT' data_type = 'CLNT' key_indicator = abap_true )
                                         ( field_name = 'FIELD1' data_type = 'CHAR' length = '10' key_indicator = abap_true )
                                         ( field_name = 'FIELD2' data_element = 'ZDS_CJS_EMAIL_FROM' ) )
      ).

    ENDIF.

    " Update Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_update = abap_true.

      l_response = me->update(
        EXPORTING
          i_table_name        = 'ZTB_CJS_XCO_TEST'
          i_description       = 'XCO Test Upd'
          i_customizing_table = abap_false
          i_size_category     = 3
          i_transport_request = 'TRLK900008'
          i_t_fields          = VALUE #( ( field_name = 'CLIENT' data_type = 'CLNT' key_indicator = abap_true )
                                         ( field_name = 'FIELD1' data_type = 'CHAR' length = '10' key_indicator = abap_true )
                                         ( field_name = 'FIELD2' data_element = 'ZDE_CJS_EMAIL_FROM' )
                                         ( field_name = 'FIELD3' data_element = 'ZDE_CJS_EMAIL_FROM' ) )
      ).

    ENDIF.

    " Delete Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_delete = abap_true.

      l_response = me->delete(
        EXPORTING
          i_table_name        = 'ztb_cjs_xco_del'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.
ENDCLASS.
