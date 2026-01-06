CLASS ycl_aaic_message_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create_message
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_message_text      TYPE bapiret2-message
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS update_message
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_message_text      TYPE bapiret2-message
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS read_message
      IMPORTING
                i_message_class   TYPE bapiret2-id
                i_message_number  TYPE bapiret2-number
      RETURNING VALUE(r_response) TYPE string.

    METHODS read_all_messages
      IMPORTING
                i_message_class   TYPE bapiret2-id
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_message_class_tools IMPLEMENTATION.

  METHOD create_message.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-msag->create_patch_operation( ).

    DATA(lo_object) = lo_patch_operation->add_object( l_message_class_name ).

    lo_object->for-insert->add_message( i_message_number )->set_short_text( CONV #( i_message_text ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Message `{ i_message_number }` created successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error: the message `{ i_message_number }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->msag.

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

  METHOD update_message.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-msag->create_patch_operation( ).

    DATA(lo_object) = lo_patch_operation->add_object( l_message_class_name ).

    lo_object->for-update->message( i_message_number )->set_short_text( CONV #( i_message_text ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Message `{ i_message_number }` updated successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error: the message `{ i_message_number }` was not updated.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->msag.

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

  METHOD read_all_messages.

  ENDMETHOD.

  METHOD read_message.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_read_all) = abap_false.

    "Create Message
    IF l_create = abap_true.

      l_response = me->create_message(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '001'
          i_message_text      = 'Message 001'
          i_transport_request = 'TRLK900028'
      ).

    ENDIF.

    IF l_update = abap_true.

      l_response = me->update_message(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '001'
          i_message_text      = 'Message 001...'
          i_transport_request = 'TRLK900028'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
