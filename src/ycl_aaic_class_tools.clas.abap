CLASS ycl_aaic_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS change_method
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_class_tools IMPLEMENTATION.

  METHOD create.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-clas->add_object( l_class_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( 'Customer implementation' ).

    lo_specification->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).

    TRY.

        lo_put_operation->execute( ).

        r_response = |Class `{ l_class_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error: the class `{ l_class_name }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->clas.

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

  METHOD add_method.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    DATA(lo_method_definition) = lo_patch_operation_object->for-insert->definition->section-public->add_method( l_method_name
      )->set_short_description( i_description ).

    lo_method_definition->add_importing_parameter( 'I_P1' )->set_pass_by_reference( )->set_type( xco_cp_abap=>type-built_in->string ).
    lo_method_definition->add_importing_parameter( 'I_P2' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>data_element( 'ZDE_CJS_EMAIL_FROM' ) ).
    lo_method_definition->add_importing_parameter( 'I_S_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>structure( 'ZST_CJS_STRUC_XCO_TEST' ) ).
    lo_method_definition->add_importing_parameter( 'I_T_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>table_type( 'ZTT_TEST_CJS_XCO_LIBRARY_12' ) ).

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( VALUE #( ( |"BINGO!| ) ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->clas.

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

  METHOD change_method.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

    lo_method_definition->for-insert->add_importing_parameter( 'I_P2' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>data_element( 'ZDE_CJS_EMAIL_FROM' ) ).
    lo_method_definition->for-insert->add_importing_parameter( 'I_S_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>structure( 'ZST_CJS_STRUC_XCO_TEST' ) ).
    lo_method_definition->for-insert->add_importing_parameter( 'I_T_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>table_type( 'ZTT_TEST_CJS_XCO_LIBRARY_12' ) ).

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( VALUE #( ( |"BINGO!| ) ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->clas.

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
    DATA(l_add_method) = abap_true.

    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_description       = 'XCO ABAP Class generation'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
      ).

    ENDIF.

    IF l_add_method = abap_true.

      l_response = me->change_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M2'
          i_description       = 'Method 2'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
        ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
