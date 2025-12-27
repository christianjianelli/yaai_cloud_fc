CLASS ycl_aaic_domain_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_domain_name        TYPE string
                i_description        TYPE yde_aaic_fc_description
                i_data_type          TYPE yde_aaic_fc_data_type
                i_length             TYPE yde_aaic_fc_length OPTIONAL
                i_decimals           TYPE yde_aaic_fc_decimals OPTIONAL
                i_case_sensitive     TYPE yde_aaic_fc_case_sensitive OPTIONAL
                i_conversion_routine TYPE yde_aaic_fc_conversion_routine OPTIONAL
                i_transport_request  TYPE yde_aaic_fc_transport_request
                i_package            TYPE yde_aaic_fc_package
                i_t_fixed_values     TYPE ytt_aaic_fc_domain_fixed_val OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS read
      IMPORTING
                i_domain_name     TYPE string
      RETURNING VALUE(r_response) TYPE string.

    METHODS update
      IMPORTING
                i_domain_name        TYPE string
                i_description        TYPE yde_aaic_fc_description OPTIONAL
                i_data_type          TYPE yde_aaic_fc_data_type OPTIONAL
                i_length             TYPE yde_aaic_fc_length OPTIONAL
                i_decimals           TYPE yde_aaic_fc_decimals OPTIONAL
                i_case_sensitive     TYPE yde_aaic_fc_case_sensitive OPTIONAL
                i_conversion_routine TYPE yde_aaic_fc_conversion_routine OPTIONAL
                i_transport_request  TYPE yde_aaic_fc_transport_request
                i_package            TYPE yde_aaic_fc_package
                i_t_fixed_values     TYPE ytt_aaic_fc_domain_fixed_val OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS delete
      IMPORTING
                i_domain_name     TYPE string
      RETURNING VALUE(r_response) TYPE string.

    METHODS activate
      IMPORTING
                i_domain_name     TYPE string
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_domain_tools IMPLEMENTATION.

  METHOD create.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    CLEAR r_response.

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

    DATA(lo_domain) = xco_cp_abap_dictionary=>domain( l_domain_name ).

    IF lo_domain->exists( ).
      r_response = |Domain `{ l_domain_name }` already exists!|.
      RETURN.
    ENDIF.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-doma->add_object( l_domain_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description ).

    NEW ycl_aaic_ddic_tools_util( )->determine_format(
      EXPORTING
        i_data_type = CONV #( i_data_type )
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
      r_response = |The data type { i_data_type } is incorrect or invalid. Only ABAP built-in types are allowed. { r_response } { NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ) }|.
      RETURN.
    ENDIF.

    lo_specification->set_format( lo_format ).

    lo_specification->output_characteristics->set_case_sensitive( i_case_sensitive ).

    lo_specification->output_characteristics->set_conversion_routine( i_conversion_routine ).

    LOOP AT i_t_fixed_values INTO DATA(ls_fixed_value).

      ls_fixed_value-value = condense( to_upper( ls_fixed_value-value ) ).

      lo_specification->fixed_values->add_fixed_value( ls_fixed_value-value
        )->set_description( ls_fixed_value-description ).

    ENDLOOP.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Domain `{ l_domain_name }` created successfully!|.

        ELSE.

          r_response = |Error: the domain `{ l_domain_name }` was not created!|.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        l_contain_errors = abap_true.

        r_response = |Error! { lo_cx_xco_gen_put_exception->get_longtext( ) }|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->doma.

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

    CLEAR r_response.

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

    DATA(lo_domain) = xco_cp_abap_dictionary=>domain( l_domain_name ).

    IF lo_domain->exists( ).

      DATA(lo_content) = lo_domain->content( ).

      DATA(l_short_description) = lo_content->get_short_description( ).

      r_response = |Domain Name: { lo_domain->name }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.

    ELSE.

      IF lo_domain->exists( io_read_state = xco_cp_abap_dictionary=>object_read_state->newest_version ).

        r_response = |The domain { i_domain_name } exists but it is not active.|.

      ELSE.

        r_response = |The domain { i_domain_name } doesn't exist.|.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD update.

    CLEAR r_response.

    DATA lo_format TYPE REF TO cl_xco_ad_built_in_type.

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

    DATA(lo_domain) = xco_cp_abap_dictionary=>domain( l_domain_name ).

    IF lo_domain->exists( ) = abap_false.
      r_response = |Domain `{ l_domain_name }` doesn't exist!|.
      RETURN.
    ENDIF.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_patch_operation( ).

    DATA(lo_specification) = lo_patch_operation->for-doma->add_object( l_domain_name
      )->create_change_specification( ).

    IF i_description IS NOT INITIAL.
      lo_specification->for-update->set_short_description( i_description ).
    ENDIF.

    IF i_data_type IS NOT INITIAL.

      NEW ycl_aaic_ddic_tools_util( )->determine_format(
        EXPORTING
          i_data_type = CONV #( i_data_type )
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
        r_response = |The data type { i_data_type } is incorrect or invalid. Only ABAP built-in types are allowed. { r_response } { NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ) }|.
        RETURN.
      ENDIF.

      lo_specification->for-update->set_format( lo_format ).

    ENDIF.

    IF i_case_sensitive IS SUPPLIED.
      lo_specification->for-update->set_case_sensitive( i_case_sensitive ).
    ENDIF.

    IF i_conversion_routine IS SUPPLIED.
      lo_specification->for-update->set_conversion_routine( i_conversion_routine ).
    ENDIF.

    LOOP AT i_t_fixed_values INTO DATA(ls_fixed_value).

      ls_fixed_value-value = condense( to_upper( ls_fixed_value-value ) ).

      lo_specification->for-insert->add_fixed_value( ls_fixed_value-value
        )->set_description( ls_fixed_value-description ).

    ENDLOOP.

    TRY.

        DATA(lo_result) = lo_patch_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Domain `{ l_domain_name }` updated successfully!|.

        ELSE.

          r_response = |Error: the domain `{ l_domain_name }` was not updated!|.

        ENDIF.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        l_contain_errors = abap_true.

        r_response = |Error! { lo_cx_xco_gen_patch_exception->get_longtext( ) }|.

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

  METHOD delete.

    CLEAR r_response.

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

  ENDMETHOD.

  METHOD activate.

    CLEAR r_response.

    DATA(l_domain_name) = CONV sxco_ad_object_name( condense( to_upper( i_domain_name ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( ''
      )->for-doma->create_patch_operation( ).

    DATA(lo_change_specification) = lo_patch_operation->add_object( l_domain_name
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

      r_response = |Domain `{ l_domain_name }` activated successfully!|.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_true.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_activate) = abap_false.

    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_domain_name        = 'ZDO_TEST'
          i_description        = 'XCO Domain generation test'
          i_data_type          = 'CHAR'
          i_length             = '1'
          i_decimals           = '1'
*          i_case_sensitive     = abap_true
*          i_conversion_routine = 'ZXPRO'
          i_transport_request  = ''
          i_package            = 'ZCHRJS'
        i_t_fixed_values     = VALUE #( ( value = 'A' description = 'VALUE A' )
                                        ( value = 'B' description = 'VALUE B' ) )
      ).

      out->write( l_response ).

    ENDIF.

    IF l_read = abap_true.

      l_response = me->read( 'ZDO_CJS_INACTIVE' ).

      out->write( l_response ).

    ENDIF.

    IF l_update = abap_true.

      l_response = me->update(
        EXPORTING
          i_domain_name        = 'ZDO_TEST'
*          i_description        = 'XCO Domain generation test'
*          i_data_type          = 'CHAR'
*          i_length             = '1'
*          i_decimals           = '1'
*          i_case_sensitive     = abap_true
*          i_conversion_routine = 'ZXPRO'
          i_transport_request  = ''
          i_package            = 'ZCHRJS'
        i_t_fixed_values     = VALUE #( ( value = 'C' description = 'VALUE C' )
                                        ( value = 'D' description = 'VALUE D' ) )
      ).

      out->write( l_response ).

    ENDIF.

    IF l_activate = abap_true.

      l_response = me->activate( 'ZDO_CJS_INACTIVE' ).

      out->write( l_response ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
