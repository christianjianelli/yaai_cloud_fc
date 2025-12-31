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

    METHODS read
      IMPORTING
                i_table_type_name TYPE yde_aaic_fc_table_type_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS update
      IMPORTING
                i_table_type_name   TYPE yde_aaic_fc_table_type_name
                i_description       TYPE yde_aaic_fc_description
                i_row_type          TYPE yde_aaic_fc_data_type OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete
      IMPORTING
                i_table_type_name   TYPE yde_aaic_fc_table_type_name
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS search
      IMPORTING
                i_table_type_name TYPE yde_aaic_fc_table_type_name OPTIONAL
                i_description     TYPE yde_aaic_fc_description OPTIONAL
                i_package         TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response) TYPE string.

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

        lo_put_operation->execute( ).

        r_response = |Table Type `{ l_table_type_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Table Type `{ l_table_type_name }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->ttyp.

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

  METHOD read.

    DATA: l_row_type                   TYPE string,
          l_row_type_name              TYPE string,
          l_row_type_short_description TYPE string.

    CLEAR r_response.

    DATA(l_table_type_name) = CONV sxco_ad_object_name( condense( to_upper( i_table_type_name ) ) ).

    DATA(lo_table_type) = xco_cp_abap_dictionary=>table_type( l_table_type_name ).

    IF lo_table_type->exists( ).

      DATA(lo_content) = lo_table_type->content( ).

      DATA(l_short_description) = lo_content->get_short_description( ).

      DATA(lo_row_type) = lo_content->get_row_type( ).

      IF lo_row_type->is_built_in_type( ).

        DATA(lo_built_in_type) = lo_row_type->get_built_in_type( ).

        l_row_type = |Built-in Type: { lo_built_in_type->type }|.
        l_row_type = |{ l_row_type }, Length: { CONV i( lo_built_in_type->length ) }|.
        l_row_type = |{ l_row_type }, Decimals: { CONV i( lo_built_in_type->decimals ) }|.

      ENDIF.

      IF lo_row_type->is_data_element( ).

        DATA(lo_data_element) = lo_row_type->get_data_element( ).

        l_row_type = 'Data Element'.
        l_row_type_name = lo_data_element->name.

        l_row_type_short_description = lo_data_element->content( )->get_short_description( ).

      ENDIF.

      IF lo_row_type->is_structure( ).

        DATA(lo_structure) = lo_row_type->get_structure( ).

        l_row_type = 'Structure'.
        l_row_type_name = lo_structure->name.

        l_row_type_short_description = lo_structure->content( )->get_short_description( ).

      ENDIF.

      r_response = |Table Type: { l_table_type_name }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Row Type: { l_row_type }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Row Type Name: { l_row_type_name }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Row Type Description: { l_row_type_short_description }{ cl_abap_char_utilities=>newline }|.

    ELSE.

      r_response = |The Table Type { l_table_type_name } doesn't exist.|.

    ENDIF.

  ENDMETHOD.

  METHOD update.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_error) = abap_false.

    DATA(l_table_type_name) = CONV sxco_ad_object_name( condense( to_upper( i_table_type_name ) ) ).

    DATA(lo_table_type) = xco_cp_abap_dictionary=>table_type( l_table_type_name ).

    IF lo_table_type->exists( ) = abap_false.

      r_response = |The Table Type { l_table_type_name } doesn't exist.|.

      RETURN.

    ENDIF.

    DATA(lo_package) = lo_table_type->if_xco_ar_object~get_package( ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(l_row_type) = CONV sxco_ad_object_name( condense( to_upper( i_row_type ) ) ).

    DATA(lo_specification) = lo_put_operation->for-ttyp->add_object( l_table_type_name
      )->set_package( lo_package->name
      )->create_form_specification( ).

    lo_specification->set_short_description(  i_description ).

    lo_specification->set_row_type( xco_cp_abap_dictionary=>structure( l_row_type ) ).

    TRY.

        lo_put_operation->execute( ).

        r_response = |Table Type `{ l_table_type_name }` updated successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error! Table Type `{ l_table_type_name }` was not updated.|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->ttyp.

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

  METHOD delete.

    CLEAR r_response.

    DATA(l_table_type_name) = CONV sxco_ad_object_name( condense( to_upper( i_table_type_name ) ) ).

    DATA(lo_table_type) = xco_cp_abap_dictionary=>table_type( l_table_type_name ).

    IF lo_table_type->exists( ) = abap_false.

      r_response = |The Table Type { l_table_type_name } doesn't exist.|.

      RETURN.

    ENDIF.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-ttyp->create_delete_operation( ).

    lo_delete_operation->add_object( l_table_type_name ).

    TRY.

        lo_delete_operation->execute( ).

        r_response = |Table Type `{ l_table_type_name }` deleted successfully!|.

      CATCH cx_xco_gen_delete_exception INTO DATA(lo_cx_xco_gen_delete_exception).

        r_response = |Error! Table Type `{ l_table_type_name }` was not deleted.|.

        r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ lo_cx_xco_gen_delete_exception->get_longtext( ) }|.

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

    DATA lt_filters TYPE sxco_t_ar_filters.

    DATA: l_length   TYPE i,
          l_decimals TYPE i.

    CLEAR r_response.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_package) = xco_cp_abap_repository=>package->for( l_package ).

    IF i_table_type_name IS NOT INITIAL.

      DATA(l_table_type_name) = |%{ CONV sxco_ad_object_name( condense( to_upper( i_table_type_name ) ) ) }%|.

      DATA(lo_name_filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( l_table_type_name ) ).

      APPEND lo_name_filter TO lt_filters.

    ENDIF.

    DATA(lt_objects) = xco_cp_abap_repository=>objects->ttyp->where( lt_filters )->in( lo_package )->get( ).

    TRY.

        LOOP AT lt_objects INTO DATA(lo_object).

          DATA(lo_content) = lo_object->content( ).

          DATA(l_short_description) = lo_content->get_short_description( ).

          IF i_description IS NOT INITIAL.

            DATA(l_pattern) = |*{ l_short_description }*|.

            IF NOT l_short_description CP l_pattern.
              CONTINUE.
            ENDIF.

          ENDIF.

          r_response = |{ r_response }Table Type: { lo_object->name }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.

        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.
        " Ignore inactive objects
    ENDTRY.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_true.
    DATA(l_delete) = abap_false.
    DATA(l_search) = abap_false.

    " Create Table Type
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_table_type_name   = 'ZTT_CJS_STRUC_XCO_TEST'
          i_description       = 'XCO Test'
          i_row_type          = 'ZST_CJS_STRUC_XCO_TEST'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
      ).

    ENDIF.

    " Update Table Type
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_update = abap_true.

      l_response = me->update(
        EXPORTING
          i_table_type_name   = 'ZTT_CJS_STRUC_XCO_TEST'
          i_description       = 'XCO Test Upd'
          i_row_type          = 'ZST_CJS_STRUC_XCO_TST'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    " Read Table Type
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_read = abap_true.

      l_response = me->read(
        EXPORTING
          i_table_type_name = 'ZTT_TEST_CJS_XCO_LIBRARY_12'
      ).

    ENDIF.

    " Delete Table Type
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_delete = abap_true.

      l_response = me->delete(
        EXPORTING
          i_table_type_name = 'ZTT_CJS_STRUC_XCO_DEL'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    " Search Table Types
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_search = abap_true.

      l_response = me->search(
        EXPORTING
          i_table_type_name = 'LIBRARY'
*          i_description     = 'Email'
          i_package         = 'ZCHRJS'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.
ENDCLASS.
