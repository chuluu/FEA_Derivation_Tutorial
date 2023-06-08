# -*- coding: utf-8 -*-
"""
Created on Mon May 22 17:20:13 2023

@author: mbluu
"""

from manim import *
import numpy as np

CONFIG = {"include_numbers": True,
          "include_tip": False}

def function(x):
    return x**2

def deriv(x):
    return 2*x


def emphasis_box(term,color=YELLOW):
    box = SurroundingRectangle(term, corner_radius=0.2,color=color)
    emphasized_term = VGroup(term,box)
    
    return emphasized_term

def arrow_text(text,arrow_coord,color=YELLOW):
    arrow = Arrow(start=arrow_coord[0], end=arrow_coord[1], color=color)
    txt = Text(text, color=color).scale(0.5).next_to(arrow,LEFT,buff = 0.1)
    
    return VGroup(arrow,txt)

class FE_Difference_Inverse(Scene):
    def MathScene1(self,player = True):
        diffeqn = MathTex(r"\frac{dy}{dx}","=","2")
        Finite_Difference = MathTex(r"\frac{dy}{dx} =", r"\frac{y_{i+1} - y_{i-1}}{2dx}").next_to(diffeqn,DOWN)

        fe_difference_eqn   = MathTex(r" {y_{i+1}"," -",r" y_{i-1} \over ","2dx}","=","2")
        fe_difference_eqn_2 = MathTex(r"y_{i+1}","-", r"y_{i-1}","=","4dx")
        fe_difference_eqn_3 = MathTex(r"y_{i+1}", r"=" ,"4dx","+",r" y_{i-1}")
        fe_difference_eqn_3 = MathTex(r"y_{i+1}", r"=" ,"4dx","+",r" y_{i-1}")

        Explain_text_PD = Text("Point difference ").scale(0.6).next_to(fe_difference_eqn_3[2],UP).set_color(ORANGE)
        Explain_text_IC = Text("Boundary conditions").scale(0.6).next_to(fe_difference_eqn_3[-1],DOWN).set_color(YELLOW)
        Explain_text_eqn = arrow_text("Original Function",[LEFT,RIGHT],color=BLUE)
        Explain_text_eqn.next_to(fe_difference_eqn_3[0],LEFT)
        
        diffeqn_boxed =emphasis_box(fe_difference_eqn[-1])
        diffeqn_boxed_PD =emphasis_box(fe_difference_eqn_3[2],ORANGE)
        diffeqn_boxed_IC =emphasis_box(fe_difference_eqn_3[-1],YELLOW)

        func_tot = MathTex(r"y_{i+1}", r"=", r"\frac{dy}{dx}","2dx", "+", r" y_{i-1}").scale(0.8).to_edge(UR).shift(RIGHT/6)
        func_tot[0].set_color(RED)
        func_tot[3].set_color(GRAY)
        func_tot[-1].set_color(MAROON)

        axes = (
            Axes(
                x_range = [0,10,2],
                x_length = 8,
                y_range = [0,20,4],
                y_length = 2.4,
                axis_config=CONFIG,
                ).to_edge(DL).set_color(GREY)
            )
        grid_labels_OG = axes.get_axis_labels(
            Tex("x").scale(0.7), Tex("y").scale(0.7)
        ).shift(DOWN/4).set_color(LIGHT_GREY)
        OG_funcbox = SurroundingRectangle(axes, corner_radius=0.2,buff = 0.4)

        axes_derivative = (
            Axes(
                x_range = [0,10,2],
                x_length = 8,
                y_range = [0,10,2],
                y_length = 2.4,
                axis_config=CONFIG,
                ).to_edge(UL).set_color(GREY)
            )
        grid_labels_deriv = axes_derivative.get_axis_labels(
            Tex("x").scale(0.7), MathTex(r"\frac{dy}{dx}").scale(0.7).set_color(LIGHT_GREY)
        ).shift(DOWN/4)
        
        Derivative_funcbox = SurroundingRectangle(axes_derivative, corner_radius=0.2,buff=0.4)
        if player == True:
            self.add(diffeqn)
            self.wait(2)
            self.play(Write(Finite_Difference),runtime=0.5)
            self.wait(1)
            self.play(TransformMatchingTex(diffeqn,fe_difference_eqn),FadeOut(Finite_Difference, target_position=diffeqn))
            self.wait(1)
            self.play(Create(diffeqn_boxed[1]))
            self.wait(1)
            self.play(FadeOut(diffeqn_boxed[1]), TransformMatchingTex(fe_difference_eqn,fe_difference_eqn_2))
            self.wait(1)
            self.play(TransformMatchingTex(fe_difference_eqn_2,fe_difference_eqn_3))
            self.wait(2)
            self.play(Create(diffeqn_boxed_PD[1]))
            self.wait(1)
            self.play(FadeIn(Explain_text_PD))
            self.wait(2)
            self.play(Create(diffeqn_boxed_IC[1]))
            self.wait(1)
            self.play(FadeIn(Explain_text_IC))
            self.wait(2)
            self.play(FadeIn(Explain_text_eqn))
            self.wait(4)
            self.play(ReplacementTransform(fe_difference_eqn_3,func_tot),
                      FadeOut(diffeqn_boxed_PD[1]),
                      FadeOut(diffeqn_boxed_IC[1]),
                      FadeOut(Explain_text_PD),
                      FadeOut(Explain_text_IC),
                      FadeOut(Explain_text_eqn),
                      FadeIn(axes),
                      FadeIn(grid_labels_OG),
                      FadeIn(axes_derivative),
                      FadeIn(grid_labels_deriv))
            self.play(Create(OG_funcbox),
                      Create(Derivative_funcbox))
            self.wait(2)
        else:
            None
            
        return [func_tot,axes,grid_labels_OG,OG_funcbox,axes_derivative,grid_labels_deriv,Derivative_funcbox]
    
    def construct(self):
        T1 = MAROON
        T2 = RED
        DX_COL = BLUE
        
        [func_tot,axes,grid_labels_OG,OG_funcbox,axes_derivative,grid_labels_deriv,Derivative_funcbox] = self.MathScene1(True)
        self.add(func_tot,axes,grid_labels_OG,OG_funcbox,axes_derivative,grid_labels_deriv,Derivative_funcbox)
        diffeqn = MathTex(r"\frac{dy}{dx}","=","2").scale(0.8).next_to(func_tot,DOWN)

        x = [0, 1]
        y = [0, 2]
        line = axes.plot_line_graph(x, y, add_vertex_dots=True)
        x = [0, 1,2,3,4,5,6,7,8,9,10]
        y = [2, 2,2,2,2,2,2,2,2,2,2]
        derivline = axes_derivative.plot_line_graph(x, y, add_vertex_dots=False)


        " Start Creating yo "
        x = ValueTracker(0)
        e = ValueTracker(1)
        dx = ValueTracker(2)
        func = axes.plot(lambda x: 2*x, x_range = [0,10], color = YELLOW)
        func_deriv = axes.plot(lambda x: 2, x_range = [0,10], color = YELLOW)

        Dot_x = always_redraw(
            lambda: Dot(color = T1).scale(1).move_to(axes.c2p(x.get_value(), 
                                                    func.underlying_function(x.get_value()))))
        
        Dot_dx = always_redraw(
            lambda: Dot(color = T2).scale(1).move_to(axes.c2p(x.get_value() + dx.get_value(), 
                                                    func.underlying_function(x.get_value() + dx.get_value()))))
        

        Dot_deriv = always_redraw(
            lambda: Dot(color = WHITE).scale(1.2).move_to(axes_derivative.c2p(e.get_value(), 
                                                    func_deriv.underlying_function(e.get_value()))))


        func_OG = always_redraw(lambda: 
                                axes.plot(lambda x: (2*x),
                                                     x_range = [1,x.get_value()+2], color = YELLOW))

        brace_dx = always_redraw( 
            lambda: BraceBetweenPoints(Dot_x.get_center() , Dot_dx.get_center(),color=GREY,buff=0.1 ))
        
        dx_val_real =always_redraw( 
            lambda:  MathTex("2dx",color=GRAY).scale(0.5).next_to(brace_dx,DOWN,buff=0).shift(RIGHT/8).rotate(PI/10))
        
        brace_dx_group = VGroup(brace_dx,dx_val_real)
        #Dot_OG = always_redraw(
        #    lambda: Dot(color = WHITE).scale(1.2).move_to(axes.c2p(e.get_value(), 
        #                                            func.underlying_function(e.get_value()))))
        
        eqn1 = always_redraw(lambda: MathTex(r"y_{i-1}",color=T1).scale(0.6).next_to(Dot_x,UP,buff=0.15))
        eqn2 = always_redraw(lambda:  MathTex(r"y_{i+1}",color=T2).scale(0.6).next_to(Dot_dx,UP,buff=0.15))
        # self.add(line,Dot_x,Dot_dx,eqn1,eqn2,derivline)
        self.play(FadeIn(line))
        self.play(FadeIn(derivline),FadeIn(diffeqn))
        self.play(FadeIn(Dot_x),FadeIn(Dot_dx),FadeIn(Dot_deriv),FadeIn(brace_dx_group))
        self.play(FadeIn(func_OG),FadeIn(eqn1),FadeIn(eqn2))
        
        for ii in range(8):
            self.play(x.animate.increment_value(1),e.animate.increment_value(1))
            self.wait(1)
            
class FE_Difference(Scene):
    def construct(self):
        T1 = MAROON
        T2 = RED
        DX_COL = BLUE
        
        OG_func = MathTex("y = x^2").to_edge(UR).shift(LEFT)

        x = ValueTracker(0)
        e = ValueTracker(1)
        dx = ValueTracker(2)
        Finite_Difference = MathTex(r"\frac{dy}{dx} =", r"\frac{y_{i+1} - y_{i-1}}{2dx}").next_to(OG_func,DOWN*2)
        Finite_Difference[1][10:13].set_color(DX_COL)
        Finite_Difference[1][0:4].set_color(T1)
        Finite_Difference[1][5:9].set_color(T2)

        axes = (
            Axes(
                x_range = [0,10,1],
                x_length = 8,
                y_range = [0,100,20],
                y_length = 2.4,
                axis_config=CONFIG,
                ).to_edge(DL).set_color(GREY)
            )
        

        grid_labels_OG = axes.get_axis_labels(
            Tex("x").scale(0.7), Tex("y").scale(0.7)
        ).shift(DOWN/4).set_color(LIGHT_GREY)
        
        func = axes.plot(lambda x: (x**2), x_range = [0,10], color = YELLOW)
        OG_funcbox = SurroundingRectangle(axes, corner_radius=0.2,buff = 0.4)
        

        
        secant = always_redraw(
            lambda: axes.get_secant_slope_group(
                x = x.get_value(),
                graph = func,
                dx = dx.get_value(),
                dx_line_color = DX_COL,
                dy_line_color = DX_COL,
                dx_label = "2dx",
                secant_line_color=TEAL,
                secant_line_length = 3,
                )
            )
        secant[2].scale(0.1)
        Dot_x = always_redraw(
            lambda: Dot(color = T1).scale(1).move_to(axes.c2p(x.get_value(), 
                                                    func.underlying_function(x.get_value()))))
        
        Dot_dx = always_redraw(
            lambda: Dot(color = T2).scale(1).move_to(axes.c2p(x.get_value() + dx.get_value(), 
                                                    func.underlying_function(x.get_value() + dx.get_value()))))
        
        # Dot_OG = always_redraw(
        #     lambda: Dot(color = WHITE).scale(1.2).move_to(axes.c2p(e.get_value(), 
        #                                             func.underlying_function(e.get_value()))))
        
        group_OG = Group(axes,OG_funcbox,func,Dot_x,Dot_dx,grid_labels_OG)

        axes_derivative = (
            Axes(
                x_range = [0,10,1],
                x_length = 8,
                y_range = [0,20,5],
                y_length = 2.4,
                axis_config=CONFIG,
                ).to_edge(UL).set_color(GREY)
            )
        
        grid_labels_deriv = axes_derivative.get_axis_labels(
            Tex("x").scale(0.7), MathTex(r"\frac{dy}{dx}").scale(0.7).set_color(LIGHT_GREY)
        ).shift(DOWN/4)
        
        derivative_func = always_redraw(lambda: 
                                        axes_derivative.plot(lambda x: (2*x),
                                                             x_range = [1,e.get_value()], color = YELLOW))
        Dot_deriv = always_redraw(
            lambda: Dot(color = WHITE).scale(1.2).move_to(axes_derivative.c2p(e.get_value(), 
                                                    derivative_func.underlying_function(e.get_value()))))
        
        Derivative_funcbox = SurroundingRectangle(axes_derivative, corner_radius=0.2,buff=0.4)
        group_deriv = Group(axes_derivative,Derivative_funcbox,derivative_func,Dot_deriv,grid_labels_deriv)
        
        DL1 = always_redraw( lambda: Dot(color = WHITE, radius=0.00001).move_to(axes.c2p(e.get_value(),28.5)))
        DL2 = always_redraw( lambda: Dot(color = WHITE, radius=0.00001).move_to(axes.c2p(e.get_value(),0 )))
        #Deriv_Line = always_redraw(lambda: DashedLine(Dot_OG,Dot_deriv))
        Deriv_Line = always_redraw(lambda: DashedLine(DL1,DL2))

        tt1 = 0 
        y1_val = Variable(tt1, 'x', num_decimal_places=3)
        on_screen_var_y1 = Variable(function(tt1), MathTex("y_{i-1}"), num_decimal_places=2).next_to(Finite_Difference,DOWN*2)
        on_screen_var_y1.label.set_color(WHITE)
        on_screen_var_y1.value.set_color(T1)
        on_screen_var_y1.add_updater(lambda a: a.tracker.set_value(function(y1_val.tracker.get_value())))

        tt2 = 2 
        y2_val = Variable(tt1, 'x', num_decimal_places=3)
        on_screen_var_y2 = Variable(function(tt2), MathTex("y_{i+1}"), num_decimal_places=2).next_to(on_screen_var_y1,DOWN*2)
        on_screen_var_y2.label.set_color(WHITE)
        on_screen_var_y2.value.set_color(T2)
        on_screen_var_y2.add_updater(lambda b: b.tracker.set_value(  function(y2_val.tracker.get_value())))

        
        tt3 = 1 
        ans_val = Variable(tt1, 'x', num_decimal_places=3)
        on_screen_var_ans = Variable( deriv(tt3), MathTex(r"\frac{dy}{dx}"), num_decimal_places=2).next_to(on_screen_var_y2,DOWN*2)
        on_screen_var_ans.label.set_color(WHITE)
        on_screen_var_ans.value.set_color(WHITE)
        on_screen_var_ans.add_updater(lambda c: c.tracker.set_value(  deriv(ans_val.tracker.get_value())))

        Derivative_func = MathTex(r"\frac{dy}{dx} = 2x").next_to(on_screen_var_ans,DOWN*3)


        self.add(secant)
        # self.add(Deriv_Line,DL1,DL2)
        self.add(on_screen_var_y1,on_screen_var_y2,on_screen_var_ans)
        self.add(group_OG,group_deriv)
        self.add(Finite_Difference,OG_func,Derivative_func)
        # self.play(x.animate.set_value(8),e.animate.set_value(9),y1_val.animate.set_value(8),y2_val.animate.set_value(10))
        self.add(y1_val.tracker.set_value(0),y2_val.tracker.set_value(2),ans_val.tracker.set_value(1))
        # self.wait(1)
        # for ii in range(8):
        #     self.play(x.animate.increment_value(1),y1_val.tracker.animate.set_value(ii+1),y2_val.tracker.animate.set_value(ii+3))
        #     self.play(e.animate.increment_value(1),ans_val.tracker.animate.set_value(ii+2))
        #     self.wait(1)

        
        

        