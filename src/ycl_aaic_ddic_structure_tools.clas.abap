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

    METHODS read
      IMPORTING
                i_structure_name  TYPE yde_aaic_fc_structure_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS update
      IMPORTING
                i_structure_name    TYPE yde_aaic_fc_structure_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_t_components      TYPE ytt_aaic_fc_struct_fields
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete
      IMPORTING
                i_structure_name    TYPE yde_aaic_fc_structure_name
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS search
      IMPORTING
                i_structure_name  TYPE yde_aaic_fc_structure_name OPTIONAL
                i_description     TYPE yde_aaic_fc_description OPTIONAL
                i_package         TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response) TYPE string.

    METHODS activate
      IMPORTING
                i_structure_name  TYPE yde_aaic_fc_structure_name
      RETURNING VALUE(r_response) TYPE string.

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

    lo_specification->set_short_description(  i_description ).

    LOOP AT i_t_components INTO DATA(ls_component).

      ls_component-field_name = condense( to_upper( ls_component-field_name ) ).

      ls_component-ref_field = condense( to_upper( ls_component-ref_field ) ).

      IF ls_component-data_element IS NOT INITIAL.

        ls_component-data_element = condense( to_upper( ls_component-data_element ) ).

        IF ls_component-ref_field IS INITIAL.

          lo_specification->add_component( ls_component-field_name
            )->set_type( xco_cp_abap_dictionary=>data_element( ls_component-data_element ) ).

        ELSE.

          lo_specification->add_component( ls_component-field_name
            )->set_type( xco_cp_abap_dictionary=>data_element( ls_component-data_element )
            )->currency_quantity->set_reference_field( ls_component-ref_field
            )->set_reference_table( l_structure_name ).

        ENDIF.

        CONTINUE.

      ENDIF.

      IF ls_component-data_type IS NOT INITIAL.

        ls_component-data_type = condense( to_upper( ls_component-data_type ) ).

        ls_component-ref_field = condense( to_upper( ls_component-ref_field ) ).

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

          IF ls_component-ref_field IS INITIAL.

            lo_specification->add_component( ls_component-field_name )->set_type( lo_format ).

          ELSE.

            lo_specification->add_component( ls_component-field_name )->set_type( lo_format
              )->currency_quantity->set_reference_field( ls_component-ref_field
              )->set_reference_table( l_structure_name ).

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
        r_response = 'For each structure field, you must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
        r_response = r_response && NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ).
      ENDIF.

      RETURN.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Structure `{ l_structure_name }` created successfully!|.

        ELSE.

          r_response = |Structure `{ l_structure_name }` was not created!|.

          DATA(lt_findings) = lo_result->findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<lo_finding>).

            r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ <lo_finding>->message->get_text( ) }|.

          ENDLOOP.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        l_contain_errors = abap_true.

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

    DATA: l_length            TYPE i,
          l_decimals          TYPE i,
          l_data_element_name TYPE string,
          l_short_description TYPE string.

    DATA(l_structure_name) = CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ).

    r_response = |Structure: { l_structure_name }{ cl_abap_char_utilities=>newline }|.

    DATA(lo_structure) = xco_cp_abap_dictionary=>structure( l_structure_name ).

    IF lo_structure->exists( ).

      DATA(lt_components_names) = lo_structure->components->all->get_names( ).

      LOOP AT lt_components_names INTO DATA(l_component_name).

        IF sy-tabix = 1.
          r_response = |{ r_response }Components:{ cl_abap_char_utilities=>newline }|.
        ENDIF.

        CLEAR: l_data_element_name, l_short_description.

        DATA(lo_dtel) = lo_structure->component( l_component_name )->content( )->get_type( )->get_data_element( ).

        IF lo_dtel IS BOUND.

          l_data_element_name = lo_dtel->content( )->data_element->name.
          l_short_description = lo_dtel->content( )->get_short_description( ).

          DATA(lo_built_in_type) = lo_dtel->content( )->get_data_type( )->get_built_in_type( ).

        ELSE.

          lo_built_in_type = lo_structure->component( l_component_name )->content( )->get_type( )->get_built_in_type( ).

        ENDIF.

        l_length = lo_built_in_type->length.
        l_decimals = lo_built_in_type->decimals.

        r_response = |{ r_response }- Component: { l_component_name }{ cl_abap_char_utilities=>newline }|.
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

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(l_structure_name) = CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ).

    DATA(lo_structure) = xco_cp_abap_dictionary=>structure( l_structure_name ).

    IF lo_structure->exists( ) = abap_false.

      r_response = |The structure { l_structure_name } doesn't exist.|.

      RETURN.

    ENDIF.

    DATA(lo_specification) = lo_put_operation->for-tabl-for-structure->add_object( l_structure_name
      )->create_form_specification( ).

    lo_specification->set_short_description(  i_description ).

    LOOP AT i_t_components INTO DATA(ls_component).

      ls_component-field_name = condense( to_upper( ls_component-field_name ) ).

      ls_component-ref_field = condense( to_upper( ls_component-ref_field ) ).

      IF ls_component-data_element IS NOT INITIAL.

        ls_component-data_element = condense( to_upper( ls_component-data_element ) ).

        IF ls_component-ref_field IS INITIAL.

          lo_specification->add_component( ls_component-field_name
            )->set_type( xco_cp_abap_dictionary=>data_element( ls_component-data_element ) ).

        ELSE.

          lo_specification->add_component( ls_component-field_name
            )->set_type( xco_cp_abap_dictionary=>data_element( ls_component-data_element )
            )->currency_quantity->set_reference_field( ls_component-ref_field
            )->set_reference_table( l_structure_name ).

        ENDIF.

        CONTINUE.

      ENDIF.

      IF ls_component-data_type IS NOT INITIAL.

        ls_component-data_type = condense( to_upper( ls_component-data_type ) ).

        ls_component-ref_field = condense( to_upper( ls_component-ref_field ) ).

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

          IF ls_component-ref_field IS INITIAL.

            lo_specification->add_component( ls_component-field_name )->set_type( lo_format ).

          ELSE.

            lo_specification->add_component( ls_component-field_name )->set_type( lo_format
              )->currency_quantity->set_reference_field( ls_component-ref_field
              )->set_reference_table( l_structure_name ).

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
        r_response = 'For each structure field, you must specify either an ABAP Data Element or an ABAP built-in type.' && cl_abap_char_utilities=>newline.
        r_response = r_response && NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ).
      ENDIF.

      RETURN.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Structure `{ l_structure_name }` updated successfully!|.

        ELSE.

          r_response = |Structure `{ l_structure_name }` was not updated!|.

          DATA(lt_findings) = lo_result->findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<lo_finding>).

            r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ <lo_finding>->message->get_text( ) }|.

          ENDLOOP.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        l_contain_errors = abap_true.

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

    DATA(l_structure_name) = CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_delete_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-tabl-for-structure->create_delete_operation( ).

    lo_delete_operation->add_object( l_structure_name ).

    TRY.

        DATA(lo_result) = lo_delete_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Structure `{ l_structure_name }` deleted successfully!|.

        ELSE.

          r_response = |Error: the Structure `{ l_structure_name }` was not deleted!|.

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

    DATA lt_filters TYPE sxco_t_ar_filters.

    DATA: l_length   TYPE i,
          l_decimals TYPE i.

    CLEAR r_response.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_package) = xco_cp_abap_repository=>package->for( l_package ).

    IF i_structure_name IS NOT INITIAL.

      DATA(l_structure_name) = |%{ CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ) }%|.

      DATA(lo_name_filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( l_structure_name ) ).

      APPEND lo_name_filter TO lt_filters.

    ENDIF.

    DATA(lt_objects) = xco_cp_abap_repository=>objects->tabl->where( lt_filters )->in( lo_package )->get( ).

    TRY.

        LOOP AT lt_objects INTO DATA(lo_object).

          IF lo_object->is_structure( ) = abap_false.
            CONTINUE.
          ENDIF.

          DATA(lo_structure) = lo_object->get_structure( ).

          DATA(lo_content) = lo_structure->content( ).

          DATA(l_short_description) = lo_content->get_short_description( ).

          IF i_description IS NOT INITIAL.

            DATA(l_pattern) = |*{ l_short_description }*|.

            IF NOT l_short_description CP l_pattern.
              CONTINUE.
            ENDIF.

          ENDIF.

          r_response = |{ r_response }Structure: { lo_object->name }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.

        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.
        " Ignore inactive objects
    ENDTRY.

  ENDMETHOD.

  METHOD activate.

    CLEAR r_response.

    DATA(l_structure_name) = CONV sxco_ad_object_name( condense( to_upper( i_structure_name ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( ''
      )->create_patch_operation( ).

    DATA(lo_change_specification) = lo_patch_operation->for-tabl-for-structure->add_object( l_structure_name
      )->create_change_specification( ).

    TRY.

        DATA(lo_result) = lo_patch_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        l_contain_errors = abap_true.

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

    IF l_contain_errors = abap_false.

      r_response = |Structure `{ l_structure_name }` activated successfully!|.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_search) = abap_false.
    DATA(l_activate) = abap_true.

    " Create Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_structure_name    = 'ZST_CJS_STRUC_XCO_TEST'
          i_description       = 'XCO Test'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
          i_t_components      = VALUE #( ( field_name = 'FIELD1' data_element = 'ZDE_CJS_EMAIL_FROM' )
                                         ( field_name = 'FIELD2' data_type = 'CHAR' length = '30' )
                                         ( field_name = 'FIELD3' data_type = 'QUAN' length = '10' decimals = 3 ref_field = 'FIELD4' )
                                         ( field_name = 'FIELD4' data_type = 'UNIT' )
                                         ( field_name = 'FIELD5' data_type = 'CURR' length = '13' decimals = 2 ref_field = 'FIELD6' )
                                         ( field_name = 'FIELD6' data_type = 'CUKY' ) )
      ).

    ENDIF.

    " Read Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_read = abap_true.

      l_response = me->read(
         EXPORTING
           i_structure_name = 'ZST_CJS_STRUC_XCO_TEST'
       ).

    ENDIF.

    " Update Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_update = abap_true.

      l_response = me->update(
        EXPORTING
          i_structure_name    = 'ZST_CJS_STRUC_XCO_TEST'
          i_description       = 'XCO Test'
          i_transport_request = 'TRLK900008'
          i_t_components      = VALUE #( ( field_name = 'FIELD1' data_element = 'ZDE_CJS_EMAIL_FROM' )
                                         ( field_name = 'FIELD2' data_type = 'CHAR' length = '50' )
                                         ( field_name = 'FIELD3' data_type = 'QUAN' length = '16' decimals = 3 ref_field = 'FIELD4' )
                                         ( field_name = 'FIELD4' data_type = 'UNIT' )
                                         ( field_name = 'FIELD5' data_type = 'CURR' length = '15' decimals = 2 ref_field = 'FIELD6' )
                                         ( field_name = 'FIELD6' data_type = 'CUKY' )
                                         ( field_name = 'FIELD7' data_type = 'CHAR' length = '10' ) )
      ).

    ENDIF.

    " Delete Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_delete = abap_true.

      l_response = me->delete(
        EXPORTING
          i_structure_name    = 'ZST_CJS_STRUC_XCO_TEST2'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    " Search Structures
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_search = abap_true.

      l_response = me->search(
        EXPORTING
          i_structure_name = 'TEST_SEARCH'
          i_package = 'ZCHRJS'
      ).

    ENDIF.

    " Activate Structure
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_activate = abap_true.

      l_response = me->activate(
        EXPORTING
          i_structure_name = 'ZST_CJS_STRUC_XCO_TEST_INACT'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.
ENDCLASS.
