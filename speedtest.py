import tkinter as tk
import time
def calculate_speed(event):
    current_time = time.time()
    text = text_widget.get("1.0", "end-1c")
    speed = len(text) / (current_time - start_time)
    speed_label.config(text=f"Typing Speed: {speed:.0f} characters per second")



root = tk.Tk()
root.title("Typing Speed App")
root.geometry("400x400")
text_widget = tk.Text(root)
text_widget.pack()
speed_label = tk.Label(root)
speed_label.pack()
text_widget.bind("<Key>", calculate_speed)

start_time = time.time()
root.mainloop()
