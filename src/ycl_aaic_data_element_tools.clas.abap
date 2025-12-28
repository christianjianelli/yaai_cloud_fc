CLASS ycl_aaic_data_element_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_data_element_name TYPE yde_aaic_fc_data_element
                i_description       TYPE yde_aaic_fc_description
                i_domain_name       TYPE yde_aaic_fc_domain OPTIONAL
                i_data_type         TYPE yde_aaic_fc_data_type OPTIONAL
                i_length            TYPE yde_aaic_fc_length OPTIONAL
                i_decimals          TYPE yde_aaic_fc_decimals OPTIONAL
                i_label_short       TYPE yde_aaic_fc_short_label OPTIONAL
                i_label_medium      TYPE yde_aaic_fc_medium_label OPTIONAL
                i_label_long        TYPE yde_aaic_fc_long_label OPTIONAL
                i_label_heading     TYPE yde_aaic_fc_heading_label OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS read
      IMPORTING
                i_data_element_name TYPE string
      RETURNING VALUE(r_response)   TYPE string.

    METHODS update
      IMPORTING
                i_data_element_name TYPE yde_aaic_fc_data_element
                i_description       TYPE yde_aaic_fc_description
                i_domain_name       TYPE yde_aaic_fc_domain OPTIONAL
                i_data_type         TYPE yde_aaic_fc_data_type OPTIONAL
                i_length            TYPE yde_aaic_fc_length OPTIONAL
                i_decimals          TYPE yde_aaic_fc_decimals OPTIONAL
                i_label_short       TYPE yde_aaic_fc_short_label OPTIONAL
                i_label_medium      TYPE yde_aaic_fc_medium_label OPTIONAL
                i_label_long        TYPE yde_aaic_fc_long_label OPTIONAL
                i_label_heading     TYPE yde_aaic_fc_heading_label OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete
      IMPORTING
                i_data_element_name TYPE string
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS search
      IMPORTING
                i_data_element_name TYPE string OPTIONAL
                i_description       TYPE yde_aaic_fc_description OPTIONAL
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS activate
      IMPORTING
                i_data_element_name TYPE string
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
      r_response = |You must specify either an ABAP Domain or an ABAP built-in type. { NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ) }|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_data_element_name) = CONV sxco_ad_object_name( condense( to_upper( i_data_element_name ) ) ).

    IF i_domain_name IS NOT INITIAL.
      l_domain_name = condense( to_upper( i_domain_name ) ).
    ELSE.

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
        r_response = |The data type { i_data_type } is incorrect or invalid. Only ABAP built-in types are allowed. { NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ) }|.
        RETURN.
      ENDIF.

    ENDIF.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-dtel->add_object( l_data_element_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description ).

    lo_specification->field_label-short->set_text( CONV #( i_label_short ) ).
    lo_specification->field_label-medium->set_text( CONV #( i_label_medium ) ).
    lo_specification->field_label-long->set_text( CONV #( i_label_long ) ).
    lo_specification->field_label-heading->set_text( CONV #( i_label_heading ) ).

    IF l_domain_name IS NOT INITIAL.

      lo_specification->set_data_type( xco_cp_abap_dictionary=>domain( l_domain_name ) ).

    ELSEIF lo_format IS BOUND.

      lo_specification->set_data_type( lo_format ).

    ENDIF.

    TRY.

        DATA(lo_result) = lo_put_operation->execute( ).

        DATA(l_contain_errors) = lo_result->findings->contain_errors( ).

        IF l_contain_errors = abap_false.

          r_response = |Data Element `{ l_data_element_name }` created successfully!|.

        ELSE.

          DATA(lt_findings) = lo_result->findings->get( ).

          LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<lo_finding>).

            IF r_response IS NOT INITIAL.
              r_response = r_response && cl_abap_char_utilities=>newline.
            ENDIF.

            LOOP AT <lo_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

              r_response = r_response && <lo_message>->get_text( ).

            ENDLOOP.

          ENDLOOP.

        ENDIF.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        l_contain_errors = abap_true.

        r_response = |Error! { lo_cx_xco_gen_put_exception->get_longtext( ) }|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->dtel.

        DATA(lt_findings_ex) = lo_findings->get( ).

        LOOP AT lt_findings_ex ASSIGNING FIELD-SYMBOL(<lo_finding_ex>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <lo_finding_ex>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message_ex>).

            r_response = r_response && <lo_message_ex>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: l_length   TYPE i,
          l_decimals TYPE i.

    CLEAR r_response.

    DATA(l_data_element_name) = CONV sxco_ad_object_name( condense( to_upper( i_data_element_name ) ) ).

    DATA(lo_data_element) = xco_cp_abap_dictionary=>data_element( l_data_element_name ).

    IF lo_data_element->exists( ).

      DATA(lo_content) = lo_data_element->content( ).

      DATA(l_short_description) = lo_content->get_short_description( ).

      DATA(lo_data_type) = lo_content->get_data_type( ).

      DATA(lo_domain) = lo_data_type->get_domain( ).

      IF lo_domain IS BOUND.

        DATA(lo_domain_content) = lo_domain->content( ).

        DATA(lo_format) = lo_domain_content->get_format( ).

        DATA(lo_built_in_type) = lo_format->get_built_in_type( ).

      ELSE.

        lo_built_in_type = lo_data_type->get_built_in_type( ).

      ENDIF.

      l_length = lo_built_in_type->length.
      l_decimals = lo_built_in_type->decimals.

      r_response = |Data Element Name: { lo_data_element->name }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.

      IF lo_domain IS BOUND.
        r_response = |{ r_response }Domain Name: { lo_domain->name }{ cl_abap_char_utilities=>newline }|.
      ENDIF.

      r_response = |{ r_response }Type: { lo_built_in_type->type }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }Length: { l_length }{ cl_abap_char_utilities=>newline }|.

      IF lo_built_in_type->type = 'DEC' OR lo_built_in_type->type = 'QUAN' OR lo_built_in_type->type = 'CURR'.
        r_response = |{ r_response }Decimals: { l_decimals }{ cl_abap_char_utilities=>newline }|.
      ENDIF.

    ELSE.

      IF lo_data_element->exists( io_read_state = xco_cp_abap_dictionary=>object_read_state->newest_version ).

        r_response = |The Data Element { l_data_element_name } exists but it is not active.|.

      ELSE.

        r_response = |The Data Element { l_data_element_name } doesn't exist.|.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD update.

    CLEAR r_response.

  ENDMETHOD.

  METHOD delete.

    CLEAR r_response.

  ENDMETHOD.

  METHOD search.

    DATA lt_filters TYPE sxco_t_ar_filters.

    DATA: l_length   TYPE i,
          l_decimals TYPE i.

    CLEAR r_response.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(lo_package) = xco_cp_abap_repository=>package->for( l_package ).

    IF i_data_element_name IS NOT INITIAL.

      DATA(l_data_element_name) = |%{ condense( to_upper( i_data_element_name ) ) }%|.

      DATA(lo_name_filter) = xco_cp_abap_repository=>object_name->get_filter( xco_cp_abap_sql=>constraint->contains_pattern( l_data_element_name ) ).

      APPEND lo_name_filter TO lt_filters.

    ENDIF.

    DATA(lt_objects) = xco_cp_abap_repository=>objects->dtel->where( lt_filters )->in( lo_package )->get( ).

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

          DATA(lo_data_type) = lo_content->get_data_type( ).

          DATA(lo_domain) = lo_data_type->get_domain( ).

          IF lo_domain IS BOUND.

            DATA(lo_domain_content) = lo_domain->content( ).

            DATA(lo_format) = lo_domain_content->get_format( ).

            DATA(lo_built_in_type) = lo_format->get_built_in_type( ).

          ELSE.

            lo_built_in_type = lo_data_type->get_built_in_type( ).

          ENDIF.

          l_length = lo_built_in_type->length.
          l_decimals = lo_built_in_type->decimals.

          r_response = |{ r_response }Data Element: { lo_object->name }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Description: { l_short_description }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Type: { lo_built_in_type->type }{ cl_abap_char_utilities=>newline }|.
          r_response = |{ r_response }Length: { l_length }{ cl_abap_char_utilities=>newline }|.

          IF lo_built_in_type->type = 'DEC' OR lo_built_in_type->type = 'QUAN' OR lo_built_in_type->type = 'CURR'.
            r_response = |{ r_response }Decimals: { l_decimals }{ cl_abap_char_utilities=>newline }|.
          ENDIF.

          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.

        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.
        " Ignore inactive objects
    ENDTRY.

  ENDMETHOD.

  METHOD activate.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_search) = abap_true.
    DATA(l_activate) = abap_false.

    " Create Domain
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_data_element_name  = 'ZDE_CJS_EMAIL_FROM'
          i_description        = 'XCO Domain generation test'
          i_domain_name        = 'ZDO_CJS_EMAIL'
*          i_data_type          = 'CHAR'
*          i_length             = '100'
*        i_decimals           = '3'
          i_label_short        = 'Email'
          i_label_medium       = 'Email From'
          i_label_long         = 'Email From .'
          i_label_heading      = 'Email From ...'
          i_transport_request  = 'TRLK900008'
          i_package            = 'ZCHRJS'
      ).

    ENDIF.

    " Read Domain
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_read = abap_true.

      l_response = me->read( i_data_element_name = 'ZDE_CJS_EMAIL_FROM' ).

    ENDIF.

    " Read Domain
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF l_search = abap_true.

      l_response = me->search(
*                     i_data_element_name =
*                     i_description       =
                     i_package           = 'ZCHRJS'
                   ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
