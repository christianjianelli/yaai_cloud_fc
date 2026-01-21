CLASS ycl_aaic_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    CONSTANTS: mc_public  TYPE string VALUE 'PUBLIC',
               mc_private TYPE string VALUE 'PRIVATE'.

    METHODS create
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_description       TYPE yde_aaic_fc_description
                i_interface_name    TYPE yde_aaic_fc_interface_name OPTIONAL
                i_superclass_name   TYPE yde_aaic_fc_superclass_name OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_description        TYPE yde_aaic_fc_description
                i_class_section      TYPE yde_aaic_fc_clas_section DEFAULT mc_public
                i_static             TYPE yde_aaic_fc_static_method DEFAULT abap_false
                i_source             TYPE string
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS change_method_implementation
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
                i_source            TYPE string
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method_parameters
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS delete_method_parameters
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_parameter OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_parameter OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_parameter OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_parameter OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS delete_method
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_attribute
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_attribute_name    TYPE yde_aaic_fc_clas_attr_name
                i_attribute_type    TYPE yde_aaic_fc_clas_attr_type
                i_class_section     TYPE yde_aaic_fc_clas_section DEFAULT mc_public
                i_static            TYPE yde_aaic_fc_static_method DEFAULT abap_false
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete_attribute
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_attribute_name    TYPE yde_aaic_fc_clas_attr_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _add_findings_to_response
      IMPORTING
                i_o_findings TYPE REF TO if_xco_gen_o_f_section
      CHANGING  ch_response  TYPE csequence.

    METHODS _get_attribute_section
      IMPORTING
        i_class_name     TYPE yde_aaic_class_name
        i_attribute_name TYPE yde_aaic_method_name
      EXPORTING
        e_section        TYPE string
        e_static         TYPE abap_bool.

    METHODS _get_method_section
      IMPORTING
        i_class_name  TYPE yde_aaic_class_name
        i_method_name TYPE yde_aaic_method_name
      EXPORTING
        e_section     TYPE string
        e_static      TYPE abap_bool.

    METHODS _get_transport_request
      IMPORTING
                i_class_name               TYPE yde_aaic_class_name
      RETURNING VALUE(r_transport_request) TYPE sxco_transport.

    METHODS _get_package
      IMPORTING
                i_class_name     TYPE yde_aaic_class_name
      RETURNING VALUE(r_package) TYPE sxco_package.

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

    lo_specification->set_short_description( i_description ).

    lo_specification->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).

    IF i_interface_name IS SUPPLIED.
      DATA(l_interface_name) = CONV sxco_ad_object_name( condense( to_upper( i_interface_name ) ) ).
      lo_specification->definition->add_interface( l_interface_name ).
    ENDIF.

    IF i_superclass_name IS SUPPLIED.
      DATA(l_superclass_name) = CONV sxco_ad_object_name( condense( to_upper( i_superclass_name ) ) ).
      lo_specification->definition->set_superclass( l_superclass_name ).
    ENDIF.

    TRY.

        lo_put_operation->execute( ).

        r_response = |Class `{ l_class_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error: the class `{ l_class_name }` was not created.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_put_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_method.

    DATA lt_source TYPE STANDARD TABLE OF string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_section) = CONV sxco_clas_method_name( to_upper( condense( i_class_section ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF i_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-insert->definition->section-private->add_method( l_method_name
          )->set_short_description( i_description ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-private->add_class_method( CONV #( l_method_name )
          )->set_short_description( i_description ).

      ENDIF.

    ELSE.

      IF i_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-public->add_method( l_method_name
          )->set_short_description( i_description ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-public->add_class_method( CONV #( l_method_name )
          )->set_short_description( i_description ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->add_importing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->add_exporting_parameter( CONV #( <ls_exporting_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changingg_params>).

      lo_method_definition->add_changing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->add_returning_parameter( CONV #( i_s_returning_param-name )
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDIF.

    SPLIT i_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( lt_source ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD change_method_implementation.

    DATA lt_source TYPE STANDARD TABLE OF string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    SPLIT i_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( lt_source ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Source code of method `{ l_method_name }` in class `{ l_class_name }` was changed successfully.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Source code of method `{ l_method_name }` in class `{ l_class_name }` was not changed.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_method_parameters.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->for-insert->add_importing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->for-insert->add_exporting_parameter( CONV #( <ls_exporting_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changingg_params>).

      lo_method_definition->for-insert->add_changing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->for-insert->add_returning_parameter( CONV #( i_s_returning_param-name )
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Parameter(s) added to method `{ l_method_name }` of class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Parameter(s) not added to method `{ l_method_name }` of class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_method_parameters.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->for-delete->add_importing_parameter( CONV #( <ls_importing_params>-name ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->for-delete->add_exporting_parameter( CONV #( <ls_exporting_params>-name ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changingg_params>).

      lo_method_definition->for-delete->add_changing_parameter( CONV #( <ls_importing_params>-name ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->for-delete->add_returning_parameter( CONV #( i_s_returning_param-name ) ).

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Parameter(s) delete from method `{ l_method_name }` of class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Parameter(s) not deleted from method `{ l_method_name }` of class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_method.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_patch_operation->execute( ).

        IF lo_result->findings->contain_errors( ).
          r_response = |Method not deleted|.
        ELSE.
          r_response = |Method was deleted|.
        ENDIF.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not deleted from class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_attribute.

    DATA lt_source TYPE STANDARD TABLE OF string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_attribute_name) = CONV sxco_clas_method_name( condense( i_attribute_name ) ).

    DATA(l_section) = CONV sxco_clas_method_name( to_upper( condense( i_class_section ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF i_static = abap_false.

        lo_patch_operation_object->for-insert->definition->section-private->add_data( CONV #( l_attribute_name )
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

      ELSE.

        lo_patch_operation_object->for-insert->definition->section-private->add_class_data( CONV #( l_attribute_name )
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

      ENDIF.

    ELSE.

      IF i_static = abap_false.

        lo_patch_operation_object->for-insert->definition->section-public->add_data( CONV #( l_attribute_name )
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

      ELSE.

        lo_patch_operation_object->for-insert->definition->section-public->add_class_data( CONV #( l_attribute_name )
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

      ENDIF.

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Attribute `{ l_attribute_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Attribute `{ l_attribute_name }` was not added to class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_attribute.

    CLEAR r_response.

    DATA lt_source TYPE STANDARD TABLE OF string.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_attribute_name) = CONV sxco_clas_method_name( condense( i_attribute_name ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_attribute_section(
      EXPORTING
        i_class_name     = l_class_name
        i_attribute_name = CONV #( l_attribute_name )
      IMPORTING
        e_section        = DATA(l_section)
        e_static         = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-private->add_data( CONV #( l_attribute_name ) ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-private->add_class_data( CONV #( l_attribute_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-public->add_data( CONV #( l_attribute_name ) ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-public->add_class_data( CONV #( l_attribute_name ) ).

      ENDIF.

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Attribute `{ l_attribute_name }` deleted from class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Attribute `{ l_attribute_name }` was not deleted from class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD _add_findings_to_response.

    DATA(lt_findings) = i_o_findings->get( ).

    LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

      IF ch_response IS NOT INITIAL.
        ch_response = ch_response && cl_abap_char_utilities=>newline.
      ENDIF.

      LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

        ch_response = ch_response && <lo_message>->get_text( ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_attribute_section.

    CLEAR: e_section, e_static.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->definition->section-public->component->data( i_attribute_name )->exists( ).
      e_section = mc_public.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->data( i_attribute_name )->exists( ).
      e_section = mc_private.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-public->component->class_data( i_attribute_name )->exists( ).
      e_section = mc_public.
      e_static = abap_true.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->class_data( i_attribute_name )->exists( ).
      e_section = mc_private.
      e_static = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _get_method_section.

    CLEAR: e_section, e_static.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->definition->section-public->component->method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_public.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_private.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-public->component->class_method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_public.
      e_static = abap_true.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->class_method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_private.
      e_static = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD _get_package.

    CLEAR r_package.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->exists( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_package) = lo_class->if_xco_ar_object~get_package( ).

    r_package = lo_package->name.

  ENDMETHOD.

  METHOD _get_transport_request.

    CLEAR r_transport_request.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->exists( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_lock) = lo_class->if_xco_cts_changeable~get_object( )->get_lock( ).

    IF lo_lock->exists( ) = abap_true.
      r_transport_request = lo_lock->get_transport( ).
    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_add_method) = abap_false.
    DATA(l_add_class_method) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_add_attribute) = abap_false.
    DATA(l_delete_attribute) = abap_false.
    DATA(l_change_method_implementation) = abap_true.
    DATA(l_add_method_parameters) = abap_false.
    DATA(l_delete_method_parameters) = abap_false.
    DATA(l_get_method_section) = abap_false.

    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_interface_name    = 'YIF_AAIC_CHAT'
          i_description       = 'XCO ABAP Class generation'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
      ).

    ENDIF.

    IF l_add_method = abap_true.

      l_response = me->add_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'YIF_AAIC_CHAT~CHAT'
          i_description       = 'Chat'
          i_transport_request = 'TRLK900008'
          i_source            = |IF 1 = 2. { cl_abap_char_utilities=>newline } ENDIF.|
          i_t_importing_params = VALUE #( ( name = 'P1' type = |YDE_AAIC_API DEFAULT 'OPENAI'| )
                                          ( name = 'P2' type = 'STRING' )
                                          ( name = 'P3' type = 'REF TO YIF_AAIC_DB OPTIONAL' ) )
        ).

    ENDIF.

    IF l_add_class_method = abap_true.

      l_response = me->add_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'CHAT'
          i_static            = abap_true
          i_description       = 'Chat'
          i_transport_request = 'TRLK900008'
          i_source            = |IF 1 = 2.{ cl_abap_char_utilities=>newline }  "Class Method{ cl_abap_char_utilities=>newline }ENDIF.|
*          i_t_importing_params = VALUE #( ( name = 'P1' type = |YDE_AAIC_API DEFAULT 'OPENAI'| )
*                                          ( name = 'P2' type = 'STRING' )
*                                          ( name = 'P3' type = 'REF TO YIF_AAIC_DB OPTIONAL' ) )
        ).

    ENDIF.

    IF l_delete = abap_true.

      l_response = me->delete_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M2'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    IF l_add_attribute = abap_true.

      l_response = me->add_attribute(
                     i_class_name        = 'ZCL_CJS_00001'
                     i_attribute_name    = 'ATTR2'
                     i_attribute_type    = 'REF TO yif_aaic_chat'
*                     i_class_section     = 'PUBLIC'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_delete_attribute = abap_true.

      l_response = me->delete_attribute(
                     i_class_name        = 'ZCL_CJS_00001'
                     i_attribute_name    = 'ATTR2'
*                     i_class_section     = 'PUBLIC'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_change_method_implementation = abap_true.

      l_response = me->change_method_implementation(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'YIF_AAIC_CHAT~CHAT'
*          i_transport_request = 'TRLK900008'
          i_source            = |IF 1 = 2. { cl_abap_char_utilities=>newline } ENDIF.|
      ).

    ENDIF.

    IF l_add_method_parameters = abap_true.

      l_response = me->add_method_parameters(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'CM1'
          i_transport_request = 'TRLK900008'
          i_t_importing_params = VALUE #( ( name = 'P4' type = |abap_boolean| ) )
      ).

    ENDIF.

    IF l_delete_method_parameters = abap_true.

      l_response = me->delete_method_parameters(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M2'
          i_transport_request = 'TRLK900008'
          i_t_importing_params = VALUE #( ( name = 'P3' ) )
      ).

    ENDIF.

    IF l_get_method_section = abap_true.

      out->write( '**get_method_section**' ).
      out->write( '------------------------------------------' ).
      out->write( 'i_class_name  = ZCL_CJS_00001' ).
      out->write( 'i_method_name = CM1' ).

      me->_get_method_section(
         EXPORTING
           i_class_name  = 'ZCL_CJS_00001'
           i_method_name = 'CM1'
         IMPORTING
           e_section = l_response
           e_static = DATA(l_static)
      ).

      IF l_static = abap_true.
        out->write( 'STATIC METHOD' ).
      ENDIF.

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.
ENDCLASS.
