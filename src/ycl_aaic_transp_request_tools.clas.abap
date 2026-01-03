CLASS ycl_aaic_transp_request_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_description     TYPE yde_aaic_fc_description
                i_package         TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response) TYPE string.

    METHODS search
      IMPORTING
                i_description     TYPE yde_aaic_fc_description OPTIONAL
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_transp_request_tools IMPLEMENTATION.

  METHOD create.

    CLEAR r_response.

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    TRY.

        DATA(ls_package) = xco_cp_abap_repository=>package->for( l_package )->read( ).

      CATCH cx_xco_ar_existence_exception INTO DATA(lo_xco_ar_existence_exception).

        r_response = |Error! The package { to_upper( i_package ) } doesn't exist.{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response }Transport request was not created.{ cl_abap_char_utilities=>newline }|.

        DATA(lo_log) = NEW ycl_aaic_log( ).

        lo_log->add( VALUE #( number = '014'
                              type = 'E'
                              message_v1 = 'ycl_aaic_transp_request_tools'
                              message_v2 = 'create'
                              message_v3 = lo_xco_ar_existence_exception->get_text( ) ) ).

        RETURN.

    ENDTRY.

    DATA(l_transport_target) = ls_package-property-transport_layer->get_transport_target( )->value.

    DATA(lo_transport_request) = xco_cp_cts=>transports->workbench( l_transport_target
      )->create_request( i_description  ).

    r_response = lo_transport_request->value.

    r_response = |Transport request `{ r_response }` created successfully!|.

  ENDMETHOD.

  METHOD search.

    CLEAR r_response.

    DATA(lo_current_user) = xco_cp=>sy->user( ).

    DATA(lo_owner_filter) = xco_cp_transport=>filter->owner( xco_cp_abap_sql=>constraint->equal( lo_current_user->name ) ).

    DATA(lo_status_filter) = xco_cp_transport=>filter->status( xco_cp_transport=>status->modifiable ).

    DATA(lt_transports) = xco_cp_cts=>transports->where( VALUE #(
      ( lo_owner_filter )
      ( lo_status_filter )
    ) )->resolve( xco_cp_transport=>resolution->request ).

    LOOP AT lt_transports INTO DATA(lo_transport).

      DATA(lo_transport_request) = lo_transport->get_request( ).

      DATA(l_transport_request) = lo_transport_request->value.

      DATA(l_description) = lo_transport_request->properties( )->get_short_description( ).

      IF r_response IS NOT INITIAL.
        r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      ENDIF.

      r_response = |{ r_response }**{ l_transport_request }** { l_description }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    out->write( me->search( ) ).

  ENDMETHOD.
ENDCLASS.
