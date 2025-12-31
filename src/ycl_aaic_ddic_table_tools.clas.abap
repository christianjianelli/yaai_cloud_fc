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
                i_delivery_class    TYPE yde_aaic_fc_delivery_class OPTIONAL
                i_data_class        TYPE yde_aaic_fc_data_class OPTIONAL
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
                i_description       TYPE yde_aaic_fc_description OPTIONAL
                i_delivery_class    TYPE yde_aaic_fc_delivery_class OPTIONAL
                i_data_class        TYPE yde_aaic_fc_data_class OPTIONAL
                i_size_category     TYPE yde_aaic_fc_size_category OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_t_fields          TYPE ytt_aaic_fc_table_fields OPTIONAL
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

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_delivery_class) = COND #( WHEN to_upper( i_delivery_class ) = 'C' THEN xco_cp_database_table=>delivery_class->c
                                      ELSE xco_cp_database_table=>delivery_class->a ).

    DATA(lo_data_class) = COND #( WHEN to_upper( i_data_class ) = 'APPL0' THEN xco_cp_database_table=>data_class->appl0
                                  WHEN to_upper( i_data_class ) = 'APPL1' THEN xco_cp_database_table=>data_class->appl1
                                  WHEN to_upper( i_data_class ) = 'APPL2' THEN xco_cp_database_table=>data_class->appl2
                                  ELSE xco_cp_database_table=>data_class->appl1 ).

    DATA(lo_size_category) = COND #( WHEN i_size_category IS INITIAL THEN xco_cp_database_table=>size_category->zero
                                     ELSE xco_cp_database_table=>size_category->for( CONV #( i_size_category ) ) ).

    DATA(lo_specification) = lo_put_operation->for-tabl-for-database_table->add_object( l_table_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description
      )->set_delivery_class( lo_delivery_class
      )->set_data_maintenance( xco_cp_database_table=>data_maintenance->allowed ).

    lo_specification->technical_settings-general_properties->set_size_category( lo_size_category ).

    lo_specification->technical_settings-general_properties->set_data_class( lo_data_class ).

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

        lo_put_operation->execute( ).

        r_response = |Table `{ l_table_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error! Table `{ l_table_name }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->tabl.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<lo_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <lo_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: l_length            TYPE i,
          l_decimals          TYPE i,
          l_data_element_name TYPE string,
          l_short_description TYPE string.

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    r_response = |Table: { l_table_name }{ cl_abap_char_utilities=>newline }|.

    DATA(lo_table) = xco_cp_abap_dictionary=>database_table( l_table_name ).

    IF lo_table->exists( ).

      l_short_description = lo_table->content( )->get_short_description( ).

      r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.

      CLEAR l_short_description.

      DATA(lo_delivery_class) = lo_table->content( )->get_delivery_class( ).

      r_response = |{ r_response }Delivery Class: { lo_delivery_class->value }{ cl_abap_char_utilities=>newline }|.

      DATA(lo_technical_settings) = lo_table->content( )->get_technical_settings( ).

      r_response = |{ r_response }Data Class: { lo_technical_settings-data_class->value }{ cl_abap_char_utilities=>newline }|.

      r_response = |{ r_response }Size Category: { lo_technical_settings-size_category->value }{ cl_abap_char_utilities=>newline }|.

      DATA(lt_fields_names) = lo_table->fields->all->get_names( ).

      LOOP AT lt_fields_names INTO DATA(l_field_name).

        IF sy-tabix = 1.
          r_response = |{ r_response }Fields:{ cl_abap_char_utilities=>newline }|.
        ENDIF.

        CLEAR: l_data_element_name, l_short_description.

        DATA(lo_dtel) = lo_table->field( l_field_name )->content( )->get_type( )->get_data_element( ).

        IF lo_dtel IS BOUND.

          l_data_element_name = lo_dtel->content( )->data_element->name.
          l_short_description = lo_dtel->content( )->get_short_description( ).

          DATA(lo_built_in_type) = lo_dtel->content( )->get_data_type( )->get_built_in_type( ).

        ELSE.

          lo_built_in_type = lo_table->field( l_field_name )->content( )->get_type( )->get_built_in_type( ).

        ENDIF.

        l_length = lo_built_in_type->length.
        l_decimals = lo_built_in_type->decimals.

        r_response = |{ r_response }- Field: { l_field_name }{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response } - Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response } - Data Element: { l_data_element_name }{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response } - Type: { lo_built_in_type->type }{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response } - Length: { l_length }{ cl_abap_char_utilities=>newline }|.

        IF lo_built_in_type->type = 'DEC' OR lo_built_in_type->type = 'QUAN' OR lo_built_in_type->type = 'CURR'.
          r_response = |{ r_response }- Decimals: { l_decimals }{ cl_abap_char_utilities=>newline }|.
        ENDIF.

      ENDLOOP.

    ENDIF.

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

    DATA(lo_package) = lo_database_table->if_xco_ar_object~get_package( ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_patch_operation( ).

    IF i_delivery_class IS SUPPLIED.

      DATA(lo_delivery_class) = COND #( WHEN to_upper( i_delivery_class ) = 'C' THEN xco_cp_database_table=>delivery_class->c
                                        ELSE xco_cp_database_table=>delivery_class->a ).

    ENDIF.

    IF i_data_class IS SUPPLIED.

      DATA(lo_data_class) = COND #( WHEN to_upper( i_data_class ) = 'APPL0' THEN xco_cp_database_table=>data_class->appl0
                                    WHEN to_upper( i_data_class ) = 'APPL1' THEN xco_cp_database_table=>data_class->appl1
                                    WHEN to_upper( i_data_class ) = 'APPL2' THEN xco_cp_database_table=>data_class->appl2
                                    ELSE xco_cp_database_table=>data_class->appl1 ).
    ENDIF.

    IF i_size_category IS SUPPLIED.

      DATA(lo_size_category) = COND #( WHEN i_size_category IS INITIAL THEN xco_cp_database_table=>size_category->zero
                                       ELSE xco_cp_database_table=>size_category->for( CONV #( i_size_category ) ) ).

    ENDIF.

    DATA(lo_put_specification) = lo_put_operation->for-tabl-for-database_table->add_object( l_table_name
      )->set_package( lo_package->name
      )->create_form_specification( ).

    DATA(lo_patch_specification) = lo_patch_operation->for-tabl-for-database_table->add_object( l_table_name
      )->create_change_specification( ).

    IF i_description IS SUPPLIED.
      lo_put_specification->set_short_description( i_description ).
      lo_patch_specification->for-update->set_short_description( i_description ).
    ENDIF.

    IF lo_delivery_class IS BOUND.
      lo_put_specification->set_delivery_class( lo_delivery_class ).
      lo_patch_specification->for-update->set_delivery_class( lo_delivery_class ).
    ENDIF.

    lo_put_specification->set_data_maintenance( xco_cp_database_table=>data_maintenance->allowed ).

    IF lo_size_category IS BOUND.
      lo_put_specification->technical_settings-general_properties->set_size_category( lo_size_category ).
    ENDIF.

    IF lo_data_class IS BOUND.
      lo_put_specification->technical_settings-general_properties->set_data_class( lo_data_class ).
    ENDIF.

    LOOP AT i_t_fields INTO DATA(ls_field).

      ls_field-field_name = condense( to_upper( ls_field-field_name ) ).

      IF ls_field-data_element IS NOT INITIAL.

        ls_field-data_element = condense( to_upper( ls_field-data_element ) ).

        DATA(lo_field) = lo_put_specification->add_field( ls_field-field_name )->set_type( xco_cp_abap_dictionary=>data_element( ls_field-data_element ) ).

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

          lo_field = lo_put_specification->add_field( ls_field-field_name )->set_type( lo_format ).

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

    IF i_t_fields IS SUPPLIED.

      TRY.

          lo_put_operation->execute( ).

          r_response = |Table `{ l_table_name }` updated successfully!|.

        CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

          r_response = |Error! Table `{ l_table_name }` was not updated.|.

          DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->tabl.

          DATA(lt_findings) = lo_findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<lo_finding>).

            IF r_response IS NOT INITIAL.
              r_response = r_response && cl_abap_char_utilities=>newline.
            ENDIF.

            LOOP AT <lo_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

              r_response = r_response && <lo_message>->get_text( ).

            ENDLOOP.

          ENDLOOP.

      ENDTRY.

    ELSE.

      TRY.

          lo_patch_operation->execute( ).

          r_response = |Table `{ l_table_name }` updated successfully!|.

        CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

          r_response = |Table `{ l_table_name }` was not updated.|.

          lo_findings = lo_cx_xco_gen_patch_exception->findings->for->tabl.

          lt_findings = lo_findings->get( ).

          LOOP AT lt_findings ASSIGNING <lo_finding>.

            IF r_response IS NOT INITIAL.
              r_response = r_response && cl_abap_char_utilities=>newline.
            ENDIF.

            LOOP AT <lo_finding>->message->if_xco_news~get_messages( ) ASSIGNING <lo_message>.

              r_response = r_response && <lo_message>->get_text( ).

            ENDLOOP.

          ENDLOOP.

      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD delete.

    CLEAR r_response.

    DATA(l_table_name) = CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-tabl-for-database_table->create_delete_operation( ).

    lo_delete_operation->add_object( l_table_name ).

    TRY.

        lo_delete_operation->execute( ).

        r_response = |Table `{ l_table_name }` deleted successfully!|.

      CATCH cx_xco_gen_delete_exception INTO DATA(lo_cx_xco_gen_delete_exception).

        r_response = |Error! Table `{ l_table_name }` was not deleted.|.

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

    IF i_table_name IS NOT INITIAL.

      DATA(l_table_name) = |%{ CONV sxco_dbt_object_name( condense( to_upper( i_table_name ) ) ) }%|.

      DATA(lo_name_filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( l_table_name ) ).

      APPEND lo_name_filter TO lt_filters.

    ENDIF.

    DATA(lt_objects) = xco_cp_abap_repository=>objects->tabl->where( lt_filters )->in( lo_package )->get( ).

    TRY.

        LOOP AT lt_objects INTO DATA(lo_object).

          IF lo_object->is_database_table( ) = abap_false.
            CONTINUE.
          ENDIF.

          DATA(lo_table) = lo_object->get_database_table( ).

          DATA(lo_content) = lo_table->content( ).

          DATA(l_short_description) = lo_content->get_short_description( ).

          IF i_description IS NOT INITIAL.

            DATA(l_pattern) = |*{ l_short_description }*|.

            IF NOT l_short_description CP l_pattern.
              CONTINUE.
            ENDIF.

          ENDIF.

          r_response = |{ r_response }Table: { lo_object->name }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.

        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.
        " Ignore inactive objects
    ENDTRY.

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

        r_response = |Table `{ l_table_name }` activated successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Table `{ l_table_name }` was not activated.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->tabl.

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
    DATA(l_delete) = abap_false.
    DATA(l_search) = abap_true.
    DATA(l_activate) = abap_false.

    " Create Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_table_name        = 'ZTB_CJS_XCO_TEST'
          i_description       = 'XCO Test'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
          i_t_fields          = VALUE #( ( field_name = 'CLIENT' data_type = 'CLNT' key_indicator = abap_true )
                                         ( field_name = 'FIELD1' data_type = 'CHAR' length = '10' key_indicator = abap_true )
                                         ( field_name = 'FIELD2' data_element = 'ZDS_CJS_EMAIL_FROM' ) )
      ).

    ENDIF.

    " Read Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_read = abap_true.

      l_response = me->read(
        EXPORTING
          i_table_name = 'ZTB_CJS_XCO_TEST'
      ).

    ENDIF.

    " Update Table
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_update = abap_true.

      l_response = me->update(
        EXPORTING
          i_table_name        = 'ZTB_CJS_XCO_TEST'
          i_description       = 'XCO Test Upd Patch'
          i_transport_request = 'TRLK900008'
          i_t_fields          = VALUE #( ( field_name = 'CLIENT' data_type = 'CLNT' key_indicator = abap_true )
                                         ( field_name = 'FIELD1' data_type = 'CHAR' length = '10' key_indicator = abap_true )
                                         ( field_name = 'FIELD2' data_element = 'ZDE_CJS_EMAIL_FROM' )
                                         ( field_name = 'FIELD3' data_element = 'ZDE_CJS_EMAIL_FROM' )
                                         ( field_name = 'FIELD4' data_element = 'ZDETEST' ) )
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

    " Search Tables
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_search = abap_true.

      l_response = me->search(
        EXPORTING
*          i_table_name = 'TEST'
          i_package = 'ZCHRJS'
      ).

    ENDIF.

    " Activate Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_activate = abap_true.

      l_response = me->activate(
        EXPORTING
          i_table_name = 'ZTB_CJS_XCO_INAC'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.
ENDCLASS.
